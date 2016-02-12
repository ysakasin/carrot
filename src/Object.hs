module Object
(
  Object(..),
  Value(..),
  call,
  genObject,
  genNilObject,
  genIntObject,
  genStringObject,
  genBoolObject,
  genFunctionObject
) where

import AST
import qualified Data.Map as Map

type Env = Map.Map String Object

data Value = IntValue Int
           | StringValue String
           | BoolValue Bool
           | ArrayValue [Object]
           | ASTValue [String] AST
           | EmptyValue
  deriving (Show)


-- Object --

data Object = Object {klass :: String, value :: Value, methods :: Map.Map String (Object -> [Object] -> Object)}
            | Return Object

instance Show Object where
  show obj = string
    where stringObj = call obj "to_s" []
          StringValue string = value stringObj

genObject = Object {klass = "Object", value = EmptyValue, methods = objMethod}
  where objMethod = Map.fromList [
          ("to_s", toSObject),
          ("true?", toBObject)
          ]

call :: Object -> String -> [Object] -> Object
call callie caller args = case looked of Just method -> method callie args
                                         Nothing -> error $ "can not find method " ++ caller
  where looked = Map.lookup caller $ methods callie

toSObject :: Object -> [Object] -> Object
toSObject self _ = genStringObject $ "#<" ++ (klass self) ++ ">"

toBObject :: Object -> [Object] -> Object
toBObject _ _ = genBoolObject True


-- nil --

genNilObject = Object {klass = "nil", value = EmptyValue, methods = nilMethod}
  where obj = genObject
        overrides = Map.fromList [
          ("to_s", toSNil),
          ("true?", toBNil)
          ]
        nilMethod = Map.union overrides $ methods obj

toSNil :: Object -> [Object] -> Object
toSNil _ _ = genStringObject "nil"

toBNil :: Object -> [Object] -> Object
toBNil _ _ = genBoolObject False


-- Int --

genIntObject :: Int -> Object
genIntObject n = Object {klass = "Int", value = IntValue n, methods = intMethod}
  where obj = genObject
        overrides = Map.fromList [
          ("to_s", toSInt),
          ("==", eqInt),
          ("+", addInt),
          ("-", subInt),
          ("*", mulInt),
          ("/", divInt),
          (">", gtInt),
          ("<", ltInt),
          ("true?", toBInt)
          ]
        intMethod = Map.union overrides $ methods obj

toSInt :: Object -> [Object] -> Object
toSInt self _ = genStringObject $ show n
  where IntValue n = value self

eqInt :: Object -> [Object] -> Object
eqInt self [right] = genBoolObject b
  where b = intBoolOp (==) (value self) (value right)

addInt :: Object -> [Object] -> Object
addInt self [right] = genIntObject n
  where n = intBinOp (+) (value self) (value right)

subInt :: Object -> [Object] -> Object
subInt self [right] = genIntObject n
  where n = intBinOp (-) (value self) (value right)

mulInt :: Object -> [Object] -> Object
mulInt self [right] = genIntObject n
  where n = intBinOp (*) (value self) (value right)

divInt :: Object -> [Object] -> Object
divInt self [right] = genIntObject n
  where n = intBinOp (div) (value self) (value right)

gtInt :: Object -> [Object] -> Object
gtInt self [right] = genBoolObject b
  where b = intBoolOp (>) (value self) (value right)

ltInt :: Object -> [Object] -> Object
ltInt self [right] = genBoolObject b
  where b = intBoolOp (<) (value self) (value right)

intBinOp f (IntValue x) (IntValue y) = f x y
intBinOp _ _ _ = error "can not add"

intBoolOp f (IntValue x) (IntValue y) = f x y
intBoolOp _ _ _ = error "can not add"

toBInt :: Object -> [Object] -> Object
toBInt self _ = genBoolObject $ n /= 0
  where IntValue n = value self


-- String --

genStringObject :: String -> Object
genStringObject s = Object {klass = "String", value = StringValue s, methods = stringMethod}
  where obj = genObject
        overrides = Map.fromList [
          ("to_s", toSString),
          ("true?", toBString)
          ]
        stringMethod = Map.union overrides $ methods obj

toSString :: Object -> [Object] -> Object
toSString self _ = self

toBString :: Object -> [Object] -> Object
toBString self _ = genBoolObject $ s /= ""
  where StringValue s = value self


-- Bool --

genBoolObject :: Bool -> Object
genBoolObject b = Object {klass = "Bool", value = BoolValue b, methods = boolMethod}
  where obj = genObject
        overrides = Map.fromList [
          ("to_s", toSBool),
          ("true?", toBBool)
          ]
        boolMethod = Map.union overrides $ methods obj

toSBool :: Object -> [Object] -> Object
toSBool self _ = if val then trueString else falseString
  where BoolValue val = value self
        trueString = genStringObject "true"
        falseString = genStringObject "false"

toBBool :: Object -> [Object] -> Object
toBBool self _ = self


-- Function --

genFunctionObject :: [String] -> AST -> Object
genFunctionObject args ast = Object {klass = "Function", value = ASTValue args ast, methods = functionMethod}
  where obj = genObject
        overrides = Map.fromList [
          ("to_s", toSFunction),
          ("call", callFunction),
          ("true?", toBFunction)
          ]
        functionMethod = Map.union overrides $ methods obj

toSFunction :: Object -> [Object] -> Object
toSFunction self _ = genStringObject $ klass self

callFunction :: Object -> [Object] -> Object
callFunction self args = self

toBFunction :: Object -> [Object] -> Object
toBFunction _ _ = genBoolObject True
