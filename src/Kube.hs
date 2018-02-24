{-# LANGUAGE DeriveGeneric #-}
{-# LANGUAGE FunctionalDependencies #-}
{-# LANGUAGE FlexibleInstances #-}
{-# LANGUAGE MultiParamTypeClasses #-}
{-# LANGUAGE OverloadedStrings #-}
{-# LANGUAGE TemplateHaskell #-}

module Kube where

import Data.Aeson
import Data.Aeson.Types
import Data.Aeson.TH
import qualified Data.Map as Map
import Data.Semigroup((<>))
import Data.Text(Text)

import GHC.Generics

import Lens.Micro(Lens', (^.), (.~), (&), ix)
import Lens.Micro.TH

import Util(dropPrefix)


-- EnvVarTypes
data TextValue = TextValue
  { _textValueName :: Text
  , _textValueValue :: Text
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_textValue" }
    ''TextValue)

data SecretKeyRef = SecretKeyRef
  { _secretKeyRefName :: Text
  , _secretKeyRefKey :: Text
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_secretKeyRef" }
    ''SecretKeyRef)

newtype ValueFrom = ValueFrom
  { _valueFromSecretKeyRef :: SecretKeyRef
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_valueFrom" }
    ''ValueFrom)

data SecretValue = SecretValue
  { _secretValueName :: Text
  , _secretValueValueFrom :: ValueFrom
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_secretValue" }
    ''SecretValue)

data EnvVar
  = EnvVarTextValue TextValue
  | EnvVarSecretValue SecretValue

$(deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''EnvVar)

-- Volume types
newtype PersistentVolumeClaim = PersistentVolumeClaim
  { _PersistentVolumeClaimClaimName :: Text }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_persistentVolumeClaim" }
    ''PersistentVolumeClaim)

data PersistentVolume = PersistentVolume
  { _persistentVolumeName :: Text
  , _persistentVolumePersistentVolumeClaim :: PersistentVolumeClaim
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_persistentVolume" }
    ''PersistentVolume)

data GitRepo = GitRepo
  { _gitRepoRepository :: Text
  , _gitRepoRevision :: Text
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_gitRepo" }
    ''GitRepo)

data GithubVolume = GithubVolume
  { _githubVolumeName :: Text
  , _githubVolumeGitRepo :: GitRepo
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_githubVolume" }
    ''GithubVolume)

newtype HostPath = HostPath
  { _hostPathPath :: Text
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_hostPath" }
    ''HostPath)

data HostVolume = HostVolume
  { _hostVolumeName :: Text
  , _hostVolumeHostPath :: HostPath
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_hostVolume" }
    ''HostVolume)

newtype Secret = Secret
  { _secretSecretName :: Text
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_secret" }
    ''Secret)


data SecretVolume = SecretVolume
  { _secretVolumeName :: Text
  , _secretVolumeSecret :: Secret
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_secretVolume" }
    ''SecretVolume)

data Volume
  = VolumePersistentVolume PersistentVolume
  | VolumeGithubVolume GithubVolume
  | VolumeHostVolume HostVolume
  | VolumeSecretVolume SecretVolume


$(deriveJSON defaultOptions { sumEncoding = UntaggedValue } ''Volume)


data VolumeMount = VolumeMount
  { _volumeMountName :: Text
  , _volumeMountMountPath :: Text
  }

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_volumeMount" }
    ''VolumeMount)


data Metadata = Metadata
  { _metadataName :: Text
  , _metadataNamespace :: Text
  , _metadataLabels :: Map.Map Text Text
  }

makeFields ''Metadata

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_metadata" }
    ''Metadata)


data Container = Container
  { _containerName :: Text
  , _containerImage :: Text
  , _containerImagePullPolicy :: Text
  , _containerCommand :: [Text]
  , _containerVolumeMounts :: [VolumeMount]
  , _containerEnv :: [EnvVar]
  }

makeFields ''Container

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_container" }
    ''Container)


data Spec = Spec
  { _specRestartPolicy :: Text
  , _specContainers :: [Container]
  , _specVolumes :: [Volume]
  }

makeFields ''Spec

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_spec" }
    ''Spec)


data Pod = Pod
  { _podApiVersion :: Text
  , _podKind :: Text
  , _podMetadata :: Metadata
  , _podSpec :: Spec
  }

makeFields ''Pod

$(deriveJSON
    defaultOptions { fieldLabelModifier = dropPrefix "_pod" }
    ''Pod)


-- constructors
makeTextValue :: Text -> Text -> EnvVar
makeTextValue name value = EnvVarTextValue
  TextValue
    { _textValueName = name
    , _textValueValue = value
    }

makeSecretValue :: Text -> Text -> Text -> EnvVar
makeSecretValue name secretName key = EnvVarSecretValue
  SecretValue
    { _secretValueName = name
    , _secretValueValueFrom = ValueFrom
      { _valueFromSecretKeyRef = SecretKeyRef
        { _secretKeyRefName = secretName
        , _secretKeyRefKey = key
        }
      }
    }

makePersistentVolume :: Text -> Volume
makePersistentVolume name = VolumePersistentVolume
  PersistentVolume
    { _persistentVolumeName = name
    , _persistentVolumePersistentVolumeClaim = PersistentVolumeClaim
      { _PersistentVolumeClaimClaimName = name
      }
    }

makeGithubVolume :: Text -> Text -> Volume
makeGithubVolume name rev = VolumeGithubVolume
  GithubVolume
    { _githubVolumeName = name
    , _githubVolumeGitRepo = GitRepo
      { _gitRepoRepository = name
      , _gitRepoRevision = rev
      }
    }

makeHostVolume :: Text -> Text -> Volume
makeHostVolume name path = VolumeHostVolume
  HostVolume
    { _hostVolumeName = name
    , _hostVolumeHostPath = HostPath
      { _hostPathPath = path
      }
    }

makeSecretVolume :: Text -> Volume
makeSecretVolume name = VolumeSecretVolume
  SecretVolume
    { _secretVolumeName = name
    , _secretVolumeSecret = Secret
      { _secretSecretName = name
      }
    }

makeMetadata :: Text -> Text -> Metadata
makeMetadata name ns = Metadata
  { _metadataName = name
  , _metadataNamespace = ns
  , _metadataLabels = Map.fromList [("app", name)]
  }

makeSpec :: Spec
makeSpec = Spec
  { _specRestartPolicy = "Never"
  , _specContainers = []
  , _specVolumes = []
  }

makeContainer :: Text -> Text -> Container
makeContainer name image = Container
  { _containerName = name
  , _containerImage = image
  , _containerImagePullPolicy = "Always"
  , _containerCommand = []
  , _containerVolumeMounts = []
  , _containerEnv = []
  }

makeVolumeMount :: Text -> Text -> VolumeMount
makeVolumeMount name mountPath = VolumeMount
  { _volumeMountName = name
  , _volumeMountMountPath = mountPath
  }

makePod :: Text -> Text -> Pod
makePod name ns = Pod
  { _podApiVersion = "v1"
  , _podKind = "Pod"
  , _podMetadata = makeMetadata name ns
  , _podSpec = makeSpec
  }


-- example pod definition
pdtPod pdtPath = makePod name "pdt"
  & spec .~
    ( makeSpec
      & containers .~
        [ makeContainer name (ecrPrefix <> "spark-pdt:2.2.1")
          & command .~ ["/spark-pdt", pdtPath]
          & volumeMounts .~
            [ makeVolumeMount "datapipeline-config" "/config"
            ]
          & env .~
            [ makeTextValue "CONFIG_ROOT" "/config/datapipeline-config"
            , makeTextValue "SHUFFLE_PARTITIONS" "400"
            , makeSecretValue
                "AWS_ACCESS_KEY_ID"
                "data-pipeline"
                "aws_access_key_id"
            , makeSecretValue
                "AWS_SECRET_ACCESS_KEY"
                "data-pipeline"
                "aws_secret_access_key"
            ]
        ]
      & volumes .~
        [ makeGithubVolume "datapipeline-config" "master"
        ]
    )
  where
    name = "spark-pdt"
    ecrPrefix = "591045937678.dkr.ecr.us-east-1.amazonaws.com/data-warehouse/"
