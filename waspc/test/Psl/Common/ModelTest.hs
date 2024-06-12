module Psl.Common.ModelTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model

-- | Corresponds to sampleBodyAst below.
sampleBodySchema :: T.Text
sampleBodySchema =
  [trimming|
    id Int @id @default(value: autoincrement())
    username String? @db.VarChar(200)
    posts Post[] @relation("UserPosts", references: [id]) @customattr
    weirdType Unsupported("weird")
    unsupportedOptionalList Sometype[]?

    @@someattr([id, username], 2 + 4, [posts])
  |]

-- | Corresponds to sampleBodySchema above.
sampleBodyAst :: Psl.Model.Body
sampleBodyAst =
  Psl.Model.Body
    [ Psl.Model.ElementField
        ( Psl.Model.Field
            { Psl.Model._name = "id",
              Psl.Model._type = Psl.Model.Int,
              Psl.Model._typeModifiers = [],
              Psl.Model._attrs =
                [ Psl.Attribute.Attribute
                    { Psl.Attribute._attrName = "id",
                      Psl.Attribute._attrArgs = []
                    },
                  Psl.Attribute.Attribute
                    { Psl.Attribute._attrName = "default",
                      Psl.Attribute._attrArgs =
                        [ Psl.Attribute.AttrArgNamed "value" (Psl.Attribute.AttrArgFunc "autoincrement")
                        ]
                    }
                ]
            }
        ),
      Psl.Model.ElementField
        ( Psl.Model.Field
            { Psl.Model._name = "username",
              Psl.Model._type = Psl.Model.String,
              Psl.Model._typeModifiers = [Psl.Model.Optional],
              Psl.Model._attrs =
                [ Psl.Attribute.Attribute
                    { Psl.Attribute._attrName = "db.VarChar",
                      Psl.Attribute._attrArgs =
                        [ Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgNumber "200")
                        ]
                    }
                ]
            }
        ),
      Psl.Model.ElementField
        ( Psl.Model.Field
            { Psl.Model._name = "posts",
              Psl.Model._type = Psl.Model.UserType "Post",
              Psl.Model._typeModifiers = [Psl.Model.List],
              Psl.Model._attrs =
                [ Psl.Attribute.Attribute
                    { Psl.Attribute._attrName = "relation",
                      Psl.Attribute._attrArgs =
                        [ Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgString "UserPosts"),
                          Psl.Attribute.AttrArgNamed "references" (Psl.Attribute.AttrArgFieldRefList ["id"])
                        ]
                    },
                  Psl.Attribute.Attribute
                    { Psl.Attribute._attrName = "customattr",
                      Psl.Attribute._attrArgs = []
                    }
                ]
            }
        ),
      Psl.Model.ElementField
        ( Psl.Model.Field
            { Psl.Model._name = "weirdType",
              Psl.Model._type = Psl.Model.Unsupported "weird",
              Psl.Model._typeModifiers = [],
              Psl.Model._attrs = []
            }
        ),
      Psl.Model.ElementField
        ( Psl.Model.Field
            { Psl.Model._name = "unsupportedOptionalList",
              Psl.Model._type = Psl.Model.UserType "Sometype",
              Psl.Model._typeModifiers = [Psl.Model.UnsupportedOptionalList],
              Psl.Model._attrs = []
            }
        ),
      Psl.Model.ElementBlockAttribute
        ( Psl.Attribute.Attribute
            { Psl.Attribute._attrName = "someattr",
              Psl.Attribute._attrArgs =
                [ Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgFieldRefList ["id", "username"]),
                  Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgUnknown "2 + 4"),
                  Psl.Attribute.AttrArgUnnamed (Psl.Attribute.AttrArgFieldRefList ["posts"])
                ]
            }
        )
    ]
