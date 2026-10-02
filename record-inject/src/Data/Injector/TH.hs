{-# LANGUAGE TemplateHaskell #-}

module Data.Injector.TH
  ( mkDependencyProvider,
  )
where

import qualified Data.Char
import qualified Data.Injector
import Data.List (find)
import Language.Haskell.TH

dependencyProviderExp :: Name -> Q (Type, Exp)
dependencyProviderExp recordConstructor = do
  (DataConI _ _ parentName) <- reify recordConstructor
  (fieldNameStrings, fieldTypes, _fieldNames) <- getRecordFieldNamesAndTypes recordConstructor
  injectorName <- newName "injector"
  getInstanceExp <- [|Data.Injector.getInstance|]
  pureExp <- [|pure|]
  ioType <- [t|IO|]
  injectorErrorType <- [t| Either Data.Injector.InjectionError |]
  recordConstructorExp <- [| Right $(pure $ ConE recordConstructor) |]
  infixAp <- [| (<*>) |]
  dependencyNames <- mapM newName fieldNameStrings
  let dependencyNamesAndTypes = zip dependencyNames fieldTypes
  let lambdaExp =
        LamE [VarP injectorName] $
          DoE Nothing $
            concat
              [ map (\(dependencyName, fieldType) -> BindS (VarP dependencyName) (SigE (AppE getInstanceExp (VarE injectorName)) (AppT ioType (AppT injectorErrorType fieldType)))) dependencyNamesAndTypes,
                [ NoBindS $ AppE pureExp $
                    foldl' (\acc dependencyName -> InfixE (Just acc) infixAp (Just $ VarE dependencyName)) recordConstructorExp dependencyNames
                ]
              ]
  lambdaType <- [t|Data.Injector.Injector IO -> IO (Either Data.Injector.InjectionError $(pure $ ConT parentName)) |]
  return $ (lambdaType, lambdaExp)

lowercaseInitialCharacter :: String -> String
lowercaseInitialCharacter (firstCharacter : xs) = (Data.Char.toLower firstCharacter) : xs
lowercaseInitialCharacter strValue = strValue

mkDependencyProvider :: Name -> Q [Dec]
mkDependencyProvider recordConstructor = do
  let name = mkName $ (lowercaseInitialCharacter $ nameBase recordConstructor) ++ "Provider"
  (providerType, providerExp) <- dependencyProviderExp recordConstructor
  return $
    [ SigD name providerType,
      FunD name [Clause [] (NormalB providerExp) []]
    ]

getRecordFieldNamesAndTypes :: Name -> Q ([String], [Type], [Name])
getRecordFieldNamesAndTypes recordConstructor = do
  (DataConI conName _conType parentName) <- reify recordConstructor
  (TyConI dec) <- reify parentName
  let constructorFields = case dec of
        (DataD _ _ _ _ constructors _) -> constructorFieldNamesAndTypes constructors
        _ -> []
  let matchingConstructors = find (\(name, _) -> name == conName) constructorFields
  let fieldNamesAndTypes = case matchingConstructors of
        (Just (_, fieldInfo)) -> fieldInfo
        Nothing -> ([], [], [])
  return fieldNamesAndTypes

constructorFieldNamesAndTypes :: [Con] -> [(Name, ([String], [Type], [Name]))]
constructorFieldNamesAndTypes constructors =
  foldr
    ( \constructor acc -> case constructor of
        RecC conName fields ->
          ( conName,
            ( fieldListFieldNames fields,
              fieldListFieldTypes fields,
              fieldListFieldNameNames fields
            )
          )
            : acc
        _ -> acc
    )
    []
    constructors

fieldListFieldNames :: [VarBangType] -> [String]
fieldListFieldNames fieldList =
  map (\(aFieldName, _, _) -> nameBase aFieldName) fieldList

fieldListFieldTypes :: [VarBangType] -> [Type]
fieldListFieldTypes fieldList =
  map (\(_, _, aFieldType) -> aFieldType) fieldList

fieldListFieldNameNames :: [VarBangType] -> [Name]
fieldListFieldNameNames fieldList =
  map (\(aFieldName, _, _) -> aFieldName) fieldList
