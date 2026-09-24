module Env.NodeDotenvParserTest where

import Control.Monad (forM_)
import Test.Hspec
import Wasp.Env.NodeDotenvParser (parseDotEnvContentLikeNodeDotenv)

-- The fixture and the expected values come from running Node's @dotenv@
-- (v16.6.1) @parse@ on the same content, since the parser is a port of it.
spec_parseDotEnvContentLikeNodeDotenv :: Spec
spec_parseDotEnvContentLikeNodeDotenv = do
  describe "parseDotEnvContentLikeNodeDotenv" $ do
    let parsed = parseDotEnvContentLikeNodeDotenv fixture

    it "returns the env vars in the order they were first defined" $
      map fst parsed `shouldBe` map fst expected

    describe "parses each value like dotenv does" $
      forM_ expected $ \(name, value) ->
        it name $ lookup name parsed `shouldBe` Just value

    it "returns nothing for empty content" $
      parseDotEnvContentLikeNodeDotenv "" `shouldBe` []

    it "uses the last definition of an env var defined more than once" $
      parseDotEnvContentLikeNodeDotenv "A=1\nB=2\nA=3\n" `shouldBe` [("A", "3"), ("B", "2")]

    it "skips lines that are not env var definitions" $
      parseDotEnvContentLikeNodeDotenv "# comment\nnot a definition\n=nokey\nKEY=value\n" `shouldBe` [("KEY", "value")]

fixture :: String
fixture = "BASIC=basic\nAFTER_LINE=after_line\nEMPTY=\nEMPTY_SINGLE_QUOTES=''\nEMPTY_DOUBLE_QUOTES=\"\"\nEMPTY_BACKTICKS=``\nSINGLE_QUOTES='single_quotes'\nSINGLE_QUOTES_SPACED='    single quotes    '\nDOUBLE_QUOTES=\"double_quotes\"\nDOUBLE_QUOTES_SPACED=\"    double quotes    \"\nDOUBLE_QUOTES_INSIDE_SINGLE='double \"quotes\" work inside single quotes'\nDOUBLE_QUOTES_WITH_NO_SPACE_BRACKET=\"{ port: $MONGOLAB_PORT}\"\nSINGLE_QUOTES_INSIDE_DOUBLE=\"single 'quotes' work inside double quotes\"\nBACKTICKS_INSIDE_SINGLE='`backticks` work inside single quotes'\nBACKTICKS_INSIDE_DOUBLE=\"`backticks` work inside double quotes\"\nBACKTICKS=`backticks`\nBACKTICKS_SPACED=`    backticks    `\nDOUBLE_QUOTES_INSIDE_BACKTICKS=`double \"quotes\" work inside backticks`\nSINGLE_QUOTES_INSIDE_BACKTICKS=`single 'quotes' work inside backticks`\nDOUBLE_AND_SINGLE_QUOTES_INSIDE_BACKTICKS=`double \"quotes\" and single 'quotes' work inside backticks`\nEXPAND_NEWLINES=\"expand\\nnew\\nlines\"\nDONT_EXPAND_UNQUOTED=dontexpand\\nnewlines\nDONT_EXPAND_SQUOTED='dontexpand\\nnewlines'\n# COMMENTS=work\nINLINE_COMMENTS=inline comments # work #very #well\nINLINE_COMMENTS_SINGLE_QUOTES='inline comments outside of #singlequotes' # work\nINLINE_COMMENTS_DOUBLE_QUOTES=\"inline comments outside of #doublequotes\" # work\nINLINE_COMMENTS_BACKTICKS=`inline comments outside of #backticks` # work\nINLINE_COMMENTS_SPACE=inline comments start with a#number sign. no space required.\nEQUAL_SIGNS=equals==\nRETAIN_INNER_QUOTES={\"foo\": \"bar\"}\nRETAIN_INNER_QUOTES_AS_STRING='{\"foo\": \"bar\"}'\nRETAIN_INNER_QUOTES_AS_BACKTICKS=`{\"foo\": \"bar's\"}`\nTRIM_SPACE_FROM_UNQUOTED=    some spaced out string\nUSERNAME=therealnerdybeast@example.tld\n    SPACED_KEY = parsed\nMULTI_DOUBLE_QUOTED=\"THIS\nIS\nA\nMULTILINE\nSTRING\"\nMULTI_SINGLE_QUOTED='THIS\nIS\nA\nMULTILINE\nSTRING'\nMULTI_BACKTICKED=`THIS\nIS\nA\n\"MULTILINE'S\"\nSTRING`\nexport EXPORTED=yes\nCOLON: colon value\nDUP=first\nDUP=second\nREF=${DUP}\nCMD=$(whoami)\nESCAPED_QUOTE=\"say \\\"hi\\\"\"\nUNCLOSED=\"abc\nTRAILING_AFTER_QUOTE=\"a\" b\nHASH_IN_UNQUOTED=val#ue\ninvalid line without equals\n=novalue\nKEY.WITH-DOTS=ok\nCRLF=1\r\nCRLF2=\"a\r\nb\"\r\n"

expected :: [(String, String)]
expected =
  [ ("BASIC", "basic"),
    ("AFTER_LINE", "after_line"),
    ("EMPTY", ""),
    ("EMPTY_SINGLE_QUOTES", ""),
    ("EMPTY_DOUBLE_QUOTES", ""),
    ("EMPTY_BACKTICKS", ""),
    ("SINGLE_QUOTES", "single_quotes"),
    ("SINGLE_QUOTES_SPACED", "    single quotes    "),
    ("DOUBLE_QUOTES", "double_quotes"),
    ("DOUBLE_QUOTES_SPACED", "    double quotes    "),
    ("DOUBLE_QUOTES_INSIDE_SINGLE", "double \"quotes\" work inside single quotes"),
    ("DOUBLE_QUOTES_WITH_NO_SPACE_BRACKET", "{ port: $MONGOLAB_PORT}"),
    ("SINGLE_QUOTES_INSIDE_DOUBLE", "single 'quotes' work inside double quotes"),
    ("BACKTICKS_INSIDE_SINGLE", "`backticks` work inside single quotes"),
    ("BACKTICKS_INSIDE_DOUBLE", "`backticks` work inside double quotes"),
    ("BACKTICKS", "backticks"),
    ("BACKTICKS_SPACED", "    backticks    "),
    ("DOUBLE_QUOTES_INSIDE_BACKTICKS", "double \"quotes\" work inside backticks"),
    ("SINGLE_QUOTES_INSIDE_BACKTICKS", "single 'quotes' work inside backticks"),
    ("DOUBLE_AND_SINGLE_QUOTES_INSIDE_BACKTICKS", "double \"quotes\" and single 'quotes' work inside backticks"),
    ("EXPAND_NEWLINES", "expand\nnew\nlines"),
    ("DONT_EXPAND_UNQUOTED", "dontexpand\\nnewlines"),
    ("DONT_EXPAND_SQUOTED", "dontexpand\\nnewlines"),
    ("INLINE_COMMENTS", "inline comments"),
    ("INLINE_COMMENTS_SINGLE_QUOTES", "inline comments outside of #singlequotes"),
    ("INLINE_COMMENTS_DOUBLE_QUOTES", "inline comments outside of #doublequotes"),
    ("INLINE_COMMENTS_BACKTICKS", "inline comments outside of #backticks"),
    ("INLINE_COMMENTS_SPACE", "inline comments start with a"),
    ("EQUAL_SIGNS", "equals=="),
    ("RETAIN_INNER_QUOTES", "{\"foo\": \"bar\"}"),
    ("RETAIN_INNER_QUOTES_AS_STRING", "{\"foo\": \"bar\"}"),
    ("RETAIN_INNER_QUOTES_AS_BACKTICKS", "{\"foo\": \"bar's\"}"),
    ("TRIM_SPACE_FROM_UNQUOTED", "some spaced out string"),
    ("USERNAME", "therealnerdybeast@example.tld"),
    ("SPACED_KEY", "parsed"),
    ("MULTI_DOUBLE_QUOTED", "THIS\nIS\nA\nMULTILINE\nSTRING"),
    ("MULTI_SINGLE_QUOTED", "THIS\nIS\nA\nMULTILINE\nSTRING"),
    ("MULTI_BACKTICKED", "THIS\nIS\nA\n\"MULTILINE'S\"\nSTRING"),
    ("EXPORTED", "yes"),
    ("COLON", "colon value"),
    ("DUP", "second"),
    ("REF", "${DUP}"),
    ("CMD", "$(whoami)"),
    ("ESCAPED_QUOTE", "say \\\"hi\\\""),
    ("UNCLOSED", "\"abc"),
    ("TRAILING_AFTER_QUOTE", "\"a\" b"),
    ("HASH_IN_UNQUOTED", "val"),
    ("KEY.WITH-DOTS", "ok"),
    ("CRLF", "1"),
    ("CRLF2", "a\nb")
  ]
