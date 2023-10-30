{-# LANGUAGE LambdaCase #-}

module CV.Types where

import Data.Aeson
import Data.Aeson.TH (deriveFromJSON)
import Optics
import Relude

data Technology
    = Angular
    | AntDesign
    | Consul
    | Docker
    | DockerCompose
    | ExpressJS
    | Flow
    | Go
    | Haskell
    | Java
    | NGINX
    | NSQ
    | NodeJS
    | OpenTracing
    | PostgreSQL
    | Prisma
    | React
    | ReactQuery
    | Redux
    | ReduxSaga
    | Remix
    | SMTP
    | SOAP
    | SpringBoot
    | StyledComponents
    | Typescript
    | Vault
    deriving (Show, Bounded, Enum)

instance ToText Technology where
    toText Angular = "Angular"
    toText AntDesign = "Ant Design"
    toText Consul = "Consul"
    toText Docker = "Docker"
    toText DockerCompose = "Docker Compose"
    toText ExpressJS = "ExpressJS"
    toText Flow = "Flow"
    toText Go = "Go"
    toText Haskell = "Haskell"
    toText Java = "Java"
    toText NGINX = "NGINX"
    toText NSQ = "NSQ"
    toText NodeJS = "NodeJS"
    toText OpenTracing = "OpenTracing"
    toText PostgreSQL = "PostgreSQL"
    toText Prisma = "Prisma"
    toText React = "React"
    toText ReactQuery = "React Query"
    toText Redux = "Redux"
    toText ReduxSaga = "Redux Saga"
    toText Remix = "Remix"
    toText SMTP = "SMTP"
    toText SOAP = "SOAP"
    toText SpringBoot = "Spring Boot"
    toText StyledComponents = "Styled Components"
    toText Typescript = "Typescript"
    toText Vault = "Vault"

instance FromJSON Technology where
    parseJSON =
        withText "Technology" \t ->
            maybe (fail $ "Unknown tech " <> toString t) pure $ inverseMap toText t

data Link = Link {title :: Text, url :: Text}
    deriving (Show)

makeFieldLabelsNoPrefix ''Link
deriveFromJSON defaultOptions ''Link

data Project = Project
    { title :: Text
    , priority :: Int
    , description :: Text
    , technologies :: [Technology]
    , links :: [Link]
    , outcome :: Maybe Text
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''Project

instance FromJSON Project where
    parseJSON = withObject "Project" \o ->
        Project
            <$> o .: "title"
            <*> o .:? "priority" .!= 1
            <*> o .: "description"
            <*> o .: "technologies"
            <*> o .:? "links" .!= []
            <*> o .:? "outcome"

data WorkExperience = WorkExperience
    { company :: Text
    , jobTitle :: Text
    , start :: Text
    , end :: Maybe Text
    , roleDescription :: Text
    , projects :: [Project]
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''WorkExperience
deriveFromJSON defaultOptions ''WorkExperience

data Skills = Skills
    { programmingLanguages :: [Text]
    , tools :: [Text]
    , languages :: [Text]
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''Skills
deriveFromJSON defaultOptions ''Skills

data About = About
    { short :: Maybe Text
    , long :: Maybe Text
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''About
deriveFromJSON defaultOptions ''About

data GeneralInfo = GeneralInfo
    { fullname :: Text
    , phonenumber :: Text
    , address :: Text
    , email :: Text
    , linkedIn :: Text
    , github :: Text
    , about :: About
    , skills :: Skills
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''GeneralInfo
deriveFromJSON defaultOptions ''GeneralInfo

data Education = Education
    { school :: Text
    , city :: Text
    , start :: Int
    , end :: Int
    , degree :: Text
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''Education
deriveFromJSON defaultOptions ''Education

data CV = CV
    { generalInfo :: GeneralInfo
    , workExperience :: [WorkExperience]
    , personalProjects :: [Project]
    , education :: [Education]
    }
    deriving (Show)

makeFieldLabelsNoPrefix ''CV
deriveFromJSON defaultOptions ''CV
