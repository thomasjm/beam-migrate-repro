{-# OPTIONS_GHC -fno-warn-missing-export-lists #-}

module Lib where

import Data.Text
import Database.Beam
import Database.Beam.Backend.SQL
import Database.Beam.Migrate as BM
import Database.Beam.Postgres

type Constraints be = (BeamSqlBackend be, BeamMigrateSqlBackend be, HasDataTypeCreatedCheck (BeamMigrateSqlBackendDataTypeSyntax be))

-- * User table

data UserT f = User { _userUsername :: Columnar f Text
                    , _userAddress  :: Columnar f Text }
  deriving (Generic, Beamable)

instance Table UserT where
  data PrimaryKey UserT f = UserId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = UserId . _userUsername

type UserId = PrimaryKey UserT Identity

referencesUsersTable :: BeamMigrateSqlBackend be => BM.Constraint be
referencesUsersTable = Constraint $ referencesConstraintSyntax "users" ["username"] Nothing Nothing Nothing

usersMigration :: (Constraints be) => Migration be (CheckedDatabaseEntity be db (TableEntity UserT))
usersMigration = createTable "users" $ User {
  _userUsername = field "username" (varchar Nothing) notNull unique
  , _userAddress = field "address" (varchar Nothing)
  }

-- * Email table

data EmailT f = Email { _emailEmail :: Columnar f Text
                      , _emailUser  :: PrimaryKey UserT f
                      , _emailUser2  :: PrimaryKey UserT f
                      }
  deriving (Generic, Beamable)

instance Table EmailT where
  data PrimaryKey EmailT f = EmailId (Columnar f Text) deriving (Generic, Beamable)
  primaryKey = EmailId . _emailEmail

type EmailId = PrimaryKey EmailT Identity

emailsMigration :: (Constraints be) => Migration be (CheckedDatabaseEntity be db (TableEntity EmailT))
emailsMigration = createTable "emails" $ Email {
  _emailEmail = field "email" (varchar Nothing) unique notNull
  , _emailUser = UserId $ field "user" (varchar Nothing) notNull referencesUsersTable
  , _emailUser2 = UserId $ field "user2" (varchar Nothing) referencesUsersTable
  }

-- * Database

data DB f = DB {
  _users :: f (TableEntity UserT)
  , _email :: f (TableEntity EmailT)
  } deriving (Generic)

initialSetup :: (Constraints be) => Migration be (CheckedDatabaseSettings be DB)
initialSetup = DB
  <$> usersMigration
  <*> emailsMigration

initialSetupStep :: forall be. (Constraints be) => MigrationSteps be () (CheckedDatabaseSettings be DB)
initialSetupStep = migrationStep "initial_setup" (const (initialSetup @be))

dbChecked :: forall be. (Constraints be) => CheckedDatabaseSettings be DB
dbChecked = evaluateDatabase (initialSetupStep @be)

deriving instance (Database Postgres) DB
