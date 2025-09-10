module Psl.Common.ModelTest where

import qualified Data.Text as T
import NeatInterpolation (trimming)
import qualified Wasp.Psl.Ast.Argument as Psl.Argument
import qualified Wasp.Psl.Ast.Attribute as Psl.Attribute
import qualified Wasp.Psl.Ast.Model as Psl.Model
import qualified Wasp.Psl.Ast.WithCtx as Psl.WithCtx

-- | Corresponds to sampleBodyAst below.
sampleBodySchema :: T.Text
sampleBodySchema =
  [trimming|
    id Int @id @default(value: autoincrement())
      username String? @db.VarChar(200) // inline comments
    posts Post[] @relation("UserPosts", references: [id]) @customattr
    weirdType Unsupported("weird")
    anotherId String @id @default(dbgenerated("gen_random_uuid()")) @db.Uuid
    someField   String[] @default([])

    @@someattr([id, username], [posts])
  |]

-- | Corresponds to sampleBodySchema above.
sampleBodyAst :: Psl.Model.Body
sampleBodyAst =
  Psl.Model.Body $
    Psl.WithCtx.empty
      <$> [ Psl.Model.ElementField
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
                              [ Psl.Argument.ArgNamed "value" (Psl.Argument.FuncExpr "autoincrement" [])
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
                              [ Psl.Argument.ArgUnnamed (Psl.Argument.NumberExpr "200")
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
                              [ Psl.Argument.ArgUnnamed (Psl.Argument.StringExpr "UserPosts"),
                                Psl.Argument.ArgNamed "references" (Psl.Argument.ArrayExpr [Psl.Argument.IdentifierExpr "id"])
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
                  { Psl.Model._name = "anotherId",
                    Psl.Model._type = Psl.Model.String,
                    Psl.Model._typeModifiers = [],
                    Psl.Model._attrs =
                      [ Psl.Attribute.Attribute
                          { Psl.Attribute._attrName = "id",
                            Psl.Attribute._attrArgs = []
                          },
                        Psl.Attribute.Attribute
                          { Psl.Attribute._attrName = "default",
                            Psl.Attribute._attrArgs =
                              [ Psl.Argument.ArgUnnamed
                                  ( Psl.Argument.FuncExpr
                                      "dbgenerated"
                                      [ Psl.Argument.ArgUnnamed $ Psl.Argument.StringExpr "gen_random_uuid()"
                                      ]
                                  )
                              ]
                          },
                        Psl.Attribute.Attribute
                          { Psl.Attribute._attrName = "db.Uuid",
                            Psl.Attribute._attrArgs =
                              []
                          }
                      ]
                  }
              ),
            Psl.Model.ElementField
              ( Psl.Model.Field
                  { Psl.Model._name = "someField",
                    Psl.Model._type = Psl.Model.String,
                    Psl.Model._typeModifiers = [Psl.Model.List],
                    Psl.Model._attrs =
                      [ Psl.Attribute.Attribute
                          { Psl.Attribute._attrName = "default",
                            Psl.Attribute._attrArgs =
                              [Psl.Argument.ArgUnnamed (Psl.Argument.ArrayExpr [])]
                          }
                      ]
                  }
              ),
            Psl.Model.ElementBlockAttribute
              ( Psl.Attribute.Attribute
                  { Psl.Attribute._attrName = "someattr",
                    Psl.Attribute._attrArgs =
                      [ Psl.Argument.ArgUnnamed
                          ( Psl.Argument.ArrayExpr
                              [ Psl.Argument.IdentifierExpr "id",
                                Psl.Argument.IdentifierExpr "username"
                              ]
                          ),
                        Psl.Argument.ArgUnnamed
                          ( Psl.Argument.ArrayExpr
                              [ Psl.Argument.IdentifierExpr "posts"
                              ]
                          )
                      ]
                  }
              )
          ]
