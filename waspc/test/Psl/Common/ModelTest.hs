module Psl.Common.ModelTest where

import qualified Wasp.Psl.Ast.Model as AST

-- | Corresponds to sampleBodyAst below.
sampleBodySchema :: String
sampleBodySchema =
  unlines
    [ "  id Int @id @default(value: autoincrement())",
      "  username String? @db.VarChar(200)",
      "  posts Post[] @relation(\"UserPosts\", references: [id]) @customattr",
      "  weirdType Unsupported(\"weird\")",
      "",
      "  @@someattr([id, username], 2 + 4, [posts])"
    ]

-- | Corresponds to sampleBodySchema above.
sampleBodyAst :: AST.Body
sampleBodyAst =
  AST.Body
    [ AST.ElementField
        ( AST.Field
            { AST._name = "id",
              AST._type = AST.Int,
              AST._typeModifiers = [],
              AST._attrs =
                [ AST.Attribute
                    { AST._attrName = "id",
                      AST._attrArgs = []
                    },
                  AST.Attribute
                    { AST._attrName = "default",
                      AST._attrArgs =
                        [ AST.AttrArgNamed "value" (AST.AttrArgFunc "autoincrement")
                        ]
                    }
                ]
            }
        ),
      AST.ElementField
        ( AST.Field
            { AST._name = "username",
              AST._type = AST.String,
              AST._typeModifiers = [AST.Optional],
              AST._attrs =
                [ AST.Attribute
                    { AST._attrName = "db.VarChar",
                      AST._attrArgs =
                        [ AST.AttrArgUnnamed (AST.AttrArgNumber "200")
                        ]
                    }
                ]
            }
        ),
      AST.ElementField
        ( AST.Field
            { AST._name = "posts",
              AST._type = AST.UserType "Post",
              AST._typeModifiers = [AST.List],
              AST._attrs =
                [ AST.Attribute
                    { AST._attrName = "relation",
                      AST._attrArgs =
                        [ AST.AttrArgUnnamed (AST.AttrArgString "UserPosts"),
                          AST.AttrArgNamed "references" (AST.AttrArgFieldRefList ["id"])
                        ]
                    },
                  AST.Attribute
                    { AST._attrName = "customattr",
                      AST._attrArgs = []
                    }
                ]
            }
        ),
      AST.ElementField
        ( AST.Field
            { AST._name = "weirdType",
              AST._type = AST.Unsupported "weird",
              AST._typeModifiers = [],
              AST._attrs = []
            }
        ),
      AST.ElementBlockAttribute
        ( AST.Attribute
            { AST._attrName = "someattr",
              AST._attrArgs =
                [ AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["id", "username"]),
                  AST.AttrArgUnnamed (AST.AttrArgUnknown "2 + 4"),
                  AST.AttrArgUnnamed (AST.AttrArgFieldRefList ["posts"])
                ]
            }
        )
    ]
