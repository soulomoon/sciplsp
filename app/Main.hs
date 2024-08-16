{-# LANGUAGE DuplicateRecordFields #-}
{-# LANGUAGE LambdaCase #-}
{-# LANGUAGE OverloadedStrings #-}

import Control.Monad.IO.Class
import Data.Text qualified as T
import Language.LSP.Protocol.Message
import Language.LSP.Protocol.Types
import Language.LSP.Server
import Proto.Scip (Document)
import qualified Data.ByteString as BS
import Data.ProtoLens (decodeMessage, MessageEnum (showEnum))
import qualified Data.Text as BS
import Data.ByteString (foldl', breakSubstring)
import qualified Data.ByteString.Char8 as BC
import Data.Maybe (catMaybes, mapMaybe, listToMaybe)
import qualified Proto.Scip as Proto
import qualified Proto.Scip_Fields as Proto
import Debug.Trace (trace, traceShow, traceShowId)
import qualified Language.LSP.Protocol.Types as LSP
import qualified Language.LSP.Server as LSP
import           Data.IntervalMap.FingerTree    (Interval (..), IntervalMap)
import qualified Data.IntervalMap.FingerTree    as IM
import Lens.Family2 ((^.))
import Proto.Scip_Fields (range, occurrences, syntaxKind)
import Data.List ((!?))
import Data.Map (Map)
import Data.Text (Text)
import qualified Data.Map as M


handlers :: DocContext -> Handlers (LspM ())
handlers docContext =
  mconcat
    [ notificationHandler SMethod_Initialized $ \_not -> do
        let params =
              ShowMessageRequestParams
                MessageType_Info
                "Turn on code lenses?"
                (Just [MessageActionItem "Turn on", MessageActionItem "Don't"])
        _ <- sendRequest SMethod_WindowShowMessageRequest params $ \case
          Right (InL (MessageActionItem "Turn on")) -> do
            let regOpts = CodeLensRegistrationOptions (InR Null) Nothing (Just False)

            _ <- registerCapability mempty SMethod_TextDocumentCodeLens regOpts $ \_req responder -> do
              let cmd = Command "Say hello" "lsp-hello-command" Nothing
                  rsp = [CodeLens (mkRange 0 0 0 100) (Just cmd) Nothing]
              responder $ Right $ InL rsp
            pure ()
          Right _ ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Info "Not turning on code lenses")
          Left err ->
            sendNotification SMethod_WindowShowMessage (ShowMessageParams MessageType_Error $ "Something went wrong!\n" <> T.pack (show err))
        pure ()
    , requestHandler SMethod_TextDocumentHover $ \req responder -> do
        let TRequestMessage _ _ _ (HoverParams _doc pos _workDone) = req
            Position _l _c' = pos
            rsp = Hover (InL ms) (Just range)
            posKind = getPosKind pos docContext
            -- ms = mkMarkdown "Hello world"
            ms = mkMarkdown $ T.pack $ show $ showEnum <$> posKind
            range = Range pos pos
        responder (Right $ InL rsp)
        -- did change configuration
    ,  notificationHandler SMethod_WorkspaceDidChangeConfiguration $ \_not -> do
       return ()
    ]

documentFilePath :: TextDocumentIdentifier -> Maybe FilePath
documentFilePath (TextDocumentIdentifier doc) = uriToFilePath doc

documentDocument :: TextDocumentIdentifier -> IO (Maybe Document)
documentDocument doc = do
  let path = documentFilePath doc
  mapM readDoc path

readDoc :: FilePath -> IO Document
readDoc path = do
    fc <- BS.readFile path
    case decodeMessage fc of
        Left e -> error $ "decode error (" ++ e ++ "):" ++ show path
        Right d -> return d


readDiags :: FilePath -> IO [Proto.Occurrence]
readDiags b = do
    diags <- BS.readFile b
    return $ decodeDiags diags

decodeDiags :: BS.ByteString -> [Proto.Occurrence]
decodeDiags b = case ll of
    (_:xs) -> catMaybes $ readDiags' xs
    _ -> []
  where ll = BC.lines b

readDiags' :: [BS.ByteString] -> [Maybe Proto.Occurrence]
readDiags' [] = []
readDiags' (x:xs) = either (const Nothing) Just (decodeMessage ys) : readDiags' (drop l xs)
  where l = read (BC.unpack x)
        ys = BC.init $ BC.unlines $ take l xs

diagnosticConvert :: Proto.Diagnostic -> LSP.Diagnostic
diagnosticConvert diag = undefined
  -- LSP.Diagnostic
  --   {
  --     -- _range = Just $ LSP.Range (Position (fromIntegral $ Proto._startLine diag) (fromIntegral $ Proto._startColumn diag)) (Position (fromIntegral $ Proto._endLine diag) (fromIntegral $ Proto._endColumn diag))
  --   -- , _severity = Just $ diagnosticSeverityConvert $ Proto._severity diag
  --   -- , _code = Proto._code diag
  --   -- , _source = Just $ Proto._source diag
  --   -- , _message = Proto._message diag
  --   -- , _relatedInformation = Nothing
  --   -- , _tags = Nothing
  --   }
    where
      -- range :: Range
      -- range = Range (Position (fromIntegral $ Proto._startLine diag) (fromIntegral $ Proto._startColumn diag)) (Position (fromIntegral $ Proto._endLine diag) (fromIntegral $ Proto._endColumn diag))
      diagnosticSeverityConvert :: Proto.Severity -> LSP.DiagnosticSeverity
      diagnosticSeverityConvert Proto.Error = LSP.DiagnosticSeverity_Error
      diagnosticSeverityConvert Proto.Warning = LSP.DiagnosticSeverity_Warning
      diagnosticSeverityConvert Proto.Information = LSP.DiagnosticSeverity_Information
      diagnosticSeverityConvert Proto.Hint = LSP.DiagnosticSeverity_Hint
      diagnosticSeverityConvert Proto.UnspecifiedSeverity = LSP.DiagnosticSeverity_Error
      diagnosticSeverityConvert (Proto.Severity'Unrecognized _) = LSP.DiagnosticSeverity_Error

      -- diagnosticSeverityConvert Proto.DiagnosticSeverity_Information = LSP.DiagnosticSeverity_Information
      -- diagnosticSeverityConvert Proto.DiagnosticSeverity_Hint = LSP.DiagnosticSeverity_Hint

symInfosSymbolMap :: [Proto.SymbolInformation] -> Map Text Proto.SymbolInformation
symInfosSymbolMap = foldr (\x acc -> M.insert (x ^. Proto.symbol) x  acc) mempty



toIntervalTree :: [Proto.Occurrence] ->  IntervalMap Position Proto.Occurrence
toIntervalTree xs = foldr (uncurry IM.insert) IM.empty intervalOcc
  where
    intervalOcc = mapMaybe toPosOcc xs
    toPosOcc :: Proto.Occurrence -> Maybe (Interval Position, Proto.Occurrence)
    toPosOcc occ = toPosOcc' occ >>= \x -> return (x, occ)

    toPosOcc' :: Proto.Occurrence -> Maybe (Interval Position)
    toPosOcc' occ =  do
      x1  <- ran !? 0
      y1  <- ran !? 2
      x2  <- ran !? 1
      y2  <- ran !? 3
      return $ Interval (Position (fromIntegral x1) (fromIntegral y1)) (Position (fromIntegral x2) (fromIntegral y2))
      where
        ran = occ ^. range

-- getSyntaxKind :: Position -> IntervalMap Position Proto.Occurrence -> Maybe Proto.Occurrence
searchFirst :: Ord v => v -> IntervalMap v b -> Maybe b
searchFirst pos intervalTree = snd <$> listToMaybe (IM.search pos intervalTree)

atPoint :: Ord v => (b -> c) -> v -> IntervalMap v b -> Maybe c
atPoint f pos intervalTree = f <$> searchFirst pos intervalTree


getSyntaxKind :: Position -> IntervalMap Position Proto.Occurrence -> Maybe Proto.SyntaxKind
getSyntaxKind = atPoint (^. syntaxKind)

target :: Position
target = Position 7 12


type DocContext = (Document, IntervalMap Position Proto.Occurrence, Map Text Proto.SymbolInformation)

getPosKind :: Position -> DocContext -> Maybe Proto.SymbolInformation'Kind
getPosKind pos (_, intervalTree, symbolMap) = do
  sym <- atPoint (^. Proto.symbol) pos intervalTree
  symbolInfo <- M.lookup sym symbolMap
  return $ symbolInfo ^. Proto.kind

main :: IO Int
main = do
  doc <- readDoc "data/src/ModuleA.hs.proto"
  -- print doc
  let intervalTree = toIntervalTree $ doc ^. occurrences
  let symbolMap = symInfosSymbolMap $ doc ^. Proto.symbols
  let docContext = (doc, intervalTree, symbolMap)
  let symbolKind = getPosKind target docContext
  let sym = atPoint (id) target intervalTree
  let symf = length $ IM.search target intervalTree
  -- let symbolInfo = sym >>= flip M.lookup symbolMap
  -- print $ showEnum <$> symbolKind
  print sym
  -- print symbolInfo
  print intervalTree
  -- print symbolMap
  runServer $
    ServerDefinition
      { parseConfig = const $ const $ Right ()
      , onConfigChange = const $ pure ()
      , defaultConfig = ()
      , configSection = "demo"
      , doInitialize = \env _req -> pure $ Right env
      , staticHandlers = \_caps -> handlers docContext
      , interpretHandler = \env -> Iso (runLspT env) liftIO
      , options = defaultOptions
      }