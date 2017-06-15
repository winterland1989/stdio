
-- | Simple parsing result, that represent respectively:
--
-- * failure: with the error message
--
-- * continuation: that need for more input data
--
-- * success: the remaining unparsed data and the parser value
--
data ParseResult a
    = ParseOK  a
    | ParseFail [ErrMsg]
    | ParseMore (Bytes -> Result a)

instance Show a => Show (Result a) where
    show (ParseFail err) = "ParseFailure: " <> show err
    show (ParseMore _)   = "ParseMore _"
    show (ParseOK a)   = "ParseOK " <> show a <> " " <> show b

-- | The continuation of the current buffer, and the error string
type Failure r = Bytes -> [ErrMsg] -> Result input r

-- | The continuation of the next buffer value, and the parsed value
type Success a r = Bytes -> a -> Result input r

-- | Simple parser structure
newtype Parser a = Parser
    { runParser :: forall r . Bytes
                           -> Failure r
                           -> Success a r
                           -> ParseResult input r }


