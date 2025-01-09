{-# LANGUAGE OverloadedStrings #-}

-- |
-- SPDX-License-Identifier: BSD-3-Clause
--
-- Module provides Types representing built-in functions and commands.
module Swarm.Language.Syntax.Constants (
  Const (..),
  allConst,
  ConstInfo (..),
  ConstDoc (..),
  ConstMeta (..),
  MBinAssoc (..),
  MUnAssoc (..),
  constInfo,
  arity,
  isCmd,
  isUserFunc,
  isOperator,
  isBuiltinFunction,
  isTangible,
  isLong,
  maxSniffRange,
  maxScoutRange,
  maxStrideRange,
  maxPathRange,
  globalMaxVolume,
) where

import Data.Aeson.Types hiding (Key)
import Data.Data (Data)
import Data.Hashable (Hashable)
import Data.Int (Int32)
import Data.List.Extra (enumerate)
import Data.Set (Set)
import Data.Set qualified as Set
import Data.Text hiding (filter, length, map)
import Data.Text qualified as T
import GHC.Generics (Generic)
import Prettyprinter (pretty)
import Swarm.Language.Syntax.CommandMetadata
import Swarm.Pretty (PrettyPrec (..), pparens)
import Swarm.Util (showT)

------------------------------------------------------------
-- Constants
------------------------------------------------------------

-- | Constants, representing various built-in functions and commands.
--
--   IF YOU ADD A NEW CONSTANT, be sure to also update:
--   1. the 'constInfo' function (below)
--   2. the capability checker ("Swarm.Language.Capability")
--   3. the type checker ("Swarm.Language.Typecheck")
--   4. the runtime ("Swarm.Game.Step")
--   5. the emacs mode syntax highlighter (@contribs/swarm-mode.el@)
--
--   GHC will warn you about incomplete pattern matches for the first
--   four, and CI will warn you about the last, so in theory it's not
--   really possible to forget.  Note you do not need to update the
--   parser or pretty-printer, since they are auto-generated from
--   'constInfo'.
data Const
  = -- Trivial actions

    -- | Do nothing.  This is different than 'Wait'
    --   in that it does not take up a time step.
    Noop
  | -- | Wait for a number of time steps without doing anything.
    Wait
  | -- | Self-destruct.
    Selfdestruct
  | -- Basic actions

    -- | Move forward one step.
    Move
  | -- | Move backward one step.
    Backup
  | -- | Measure the size of the enclosed volume
    Volume
  | -- | Describe a path to the destination.
    Path
  | -- | Push an entity forward one step.
    Push
  | -- | Move forward multiple steps.
    Stride
  | -- | Turn in some direction.
    Turn
  | -- | Grab an item from the current location.
    Grab
  | -- | Harvest an item from the current location.
    Harvest
  | -- | Scatter seeds of a plant
    Sow
  | -- | Ignite a combustible item
    Ignite
  | -- | Try to place an item at the current location.
    Place
  | -- | Obtain the relative location of another robot.
    Ping
  | -- | Give an item to another robot at the current location.
    Give
  | -- | Equip a device on oneself.
    Equip
  | -- | Unequip an equipped device, returning to inventory.
    Unequip
  | -- | Make an item.
    Make
  | -- | Sense whether we have a certain item.
    Has
  | -- | Sense whether we have a certain device equipped.
    Equipped
  | -- | Sense how many of a certain item we have.
    Count
  | -- | Act upon an entity.
    Act
  | -- | Drill through an entity.
    Drill
  | -- | Use an entity with another.
    Use
  | -- | Construct a new robot.
    Build
  | -- | Deconstruct an old robot.
    Salvage
  | -- | Reprogram a robot that has executed it's command
    --   with a new command
    Reprogram
  | -- | Emit a message.
    Say
  | -- | Listen for a message from other robots.
    Listen
  | -- | Emit a log message.
    Log
  | -- | View a certain robot.
    View
  | -- | Set color and what characters are used for display.
    Appear
  | -- | Create an entity out of thin air. Only
    --   available in creative mode.
    Create
  | -- | Tell a robot to halt.
    Halt
  | -- Sensing / generation

    -- | Get current time
    Time
  | -- Detect whether a robot is within line-of-sight in a direction
    Scout
  | -- | Get the current x, y coordinates
    Whereami
  | -- | Get the current subworld and x, y coordinates
    LocateMe
  | -- | Get the x, y coordinates of a specific waypoints by name and index
    Waypoint
  | -- | Get the list of x, y coordinates of the waypoints for a given name
    Waypoints
  | -- | Get the list of x, y coordinates of the southwest corner of all
    --   constructed structures of a given name
    Structures
  | -- | Get the width and height of a structure template
    Floorplan
  | -- | Answer whether a given entity has the given tag
    HasTag
  | -- | Get the list of entity names that are labeled with a given tag
    TagMembers
  | -- | Locate the closest instance of a given entity within the rectangle
    -- specified by opposite corners, relative to the current location.
    Detect
  | -- | Count the number of a given entity within the rectangle
    -- specified by opposite corners, relative to the current location.
    Resonate
  | -- | Count the number entities within the rectangle
    -- specified by opposite corners, relative to the current location.
    Density
  | -- | Get the distance to the closest instance of the specified entity.
    Sniff
  | -- | Get the direction to the closest instance of the specified entity.
    Chirp
  | -- | Register a location to interrupt a `wait` upon changes
    Watch
  | -- | Register a (remote) location to interrupt a `wait` upon changes
    Surveil
  | -- | Get the current heading.
    Heading
  | -- | See if we can move forward or not.
    Blocked
  | -- | Scan a nearby cell
    Scan
  | -- | Upload knowledge to another robot
    Upload
  | -- | See if a specific entity is here.
    Ishere
  | -- | Check whether the current cell is empty
    Isempty
  | -- | Get a reference to oneself
    Self
  | -- | Get the robot's parent
    Parent
  | -- | Get a reference to the base
    Base
  | -- | Meet a nearby robot
    Meet
  | -- | Meet all nearby robots
    MeetAll
  | -- | Get the robot's display name
    Whoami
  | -- | Set the robot's display name
    Setname
  | -- | Get a uniformly random integer.
    Random
  | -- Modules

    -- | Run a program loaded from a file.
    Run
  | -- Language built-ins

    -- | If-expressions.
    If
  | -- | Left injection.
    Inl
  | -- | Right injection.
    Inr
  | -- | Case analysis on a sum type.
    Case
  | -- | First projection.
    Fst
  | -- | Second projection.
    Snd
  | -- | Force a delayed evaluation.
    Force
  | -- | Return for the cmd monad.
    Return
  | -- | Try/catch block
    Try
  | -- | Undefined
    Undefined
  | -- | User error
    Fail
  | -- Arithmetic unary operators

    -- | Logical negation.
    Not
  | -- | Arithmetic negation.
    Neg
  | -- Comparison operators

    -- | Logical equality comparison
    Eq
  | -- | Logical inequality comparison
    Neq
  | -- | Logical lesser-then comparison
    Lt
  | -- | Logical greater-then comparison
    Gt
  | -- | Logical lesser-or-equal comparison
    Leq
  | -- | Logical greater-or-equal comparison
    Geq
  | -- Arithmetic binary operators

    -- | Logical or.
    Or
  | -- | Logical and.
    And
  | -- | Arithmetic addition operator
    Add
  | -- | Arithmetic subtraction operator
    Sub
  | -- | Arithmetic multiplication operator
    Mul
  | -- | Arithmetic division operator
    Div
  | -- | Arithmetic exponentiation operator
    Exp
  | -- String operators

    -- | Turn an arbitrary value into a string
    Format
  | -- | Try to turn a string into a value
    Read
  | -- | Print a string onto a printable surface
    Print
  | -- | Erase a printable surface
    Erase
  | -- | Concatenate string values
    Concat
  | -- | Count number of characters.
    Chars
  | -- | Split string into two parts.
    Split
  | -- | Get the character at an index.
    CharAt
  | -- | Create a singleton text value with the given character code.
    ToChar
  | -- Function composition with nice operators

    -- | Application operator - helps to avoid parentheses:
    --   @f $ g $ h x  =  f (g (h x))@
    AppF
  | -- Concurrency

    -- | Swap placed entity with one in inventory. Essentially atomic grab and place.
    Swap
  | -- | When executing @atomic c@, a robot will not be interrupted,
    --   that is, no other robots will execute any commands while
    --   the robot is executing @c@.
    Atomic
  | -- | Like @atomic@, but with no restriction on program size.
    Instant
  | -- Keyboard input

    -- | Create `key` values.
    Key
  | -- | Install a new keyboard input handler.
    InstallKeyHandler
  | -- God-like commands that are omnipresent or omniscient.

    -- | Teleport a robot to the given position.
    Teleport
  | -- | Relocate a robot to the given cosmic position.
    Warp
  | -- | Run a command as if you were another robot.
    As
  | -- | Find an actor by name.
    RobotNamed
  | -- | Find an actor by number.
    RobotNumbered
  | -- | Check if an entity is known.
    Knows
  deriving (Eq, Ord, Enum, Bounded, Data, Show, Generic, Hashable, FromJSON, ToJSON, FromJSONKey, ToJSONKey)

instance PrettyPrec Const where
  prettyPrec p c = pparens (p > fixity (constInfo c)) $ pretty . syntax . constInfo $ c

allConst :: [Const]
allConst = enumerate

data ConstInfo = ConstInfo
  { syntax :: Text
  , fixity :: Int
  , constMeta :: ConstMeta
  , constDoc :: ConstDoc
  , tangibility :: Tangibility
  }
  deriving (Eq, Ord, Show)

data ConstDoc = ConstDoc
  { effectInfo :: Set CommandEffect
  -- ^ NOTE: The absence of effects implies a "pure computation".
  , briefDoc :: Text
  , longDoc :: Text
  }
  deriving (Eq, Ord, Show)

data ConstMeta
  = -- | Function with arity of which some are commands
    ConstMFunc
      -- | Arity
      Int
      -- | Is a command?
      Bool
  | -- | Unary operator with fixity and associativity.
    ConstMUnOp MUnAssoc
  | -- | Binary operator with fixity and associativity.
    ConstMBinOp MBinAssoc
  deriving (Eq, Ord, Show)

-- | The meta type representing associativity of binary operator.
data MBinAssoc
  = -- |  Left associative binary operator (see 'Control.Monad.Combinators.Expr.InfixL')
    L
  | -- |   Non-associative binary operator (see 'Control.Monad.Combinators.Expr.InfixN')
    N
  | -- | Right associative binary operator (see 'Control.Monad.Combinators.Expr.InfixR')
    R
  deriving (Eq, Ord, Show)

-- | The meta type representing associativity of unary operator.
data MUnAssoc
  = -- |  Prefix unary operator (see 'Control.Monad.Combinators.Expr.Prefix')
    P
  | -- |  Suffix unary operator (see 'Control.Monad.Combinators.Expr.Suffix')
    S
  deriving (Eq, Ord, Show)

-- | Whether a command is tangible or not.  Tangible commands have
--   some kind of effect on the external world; at most one tangible
--   command can be executed per tick.  Intangible commands are things
--   like sensing commands, or commands that solely modify a robot's
--   internal state; multiple intangible commands may be executed per
--   tick.  In addition, tangible commands can have a 'Length' (either
--   'Short' or 'Long') indicating whether they require only one, or
--   possibly more than one, tick to execute.  Long commands are
--   excluded from @atomic@ blocks to avoid freezing the game.
data Tangibility = Intangible | Tangible Length
  deriving (Eq, Ord, Show, Read)

-- | For convenience, @short = Tangible Short@.
short :: Tangibility
short = Tangible Short

-- | For convenience, @long = Tangible Long@.
long :: Tangibility
long = Tangible Long

-- | The length of a tangible command.  Short commands take exactly
--   one tick to execute.  Long commands may require multiple ticks.
data Length = Short | Long
  deriving (Eq, Ord, Show, Read, Bounded, Enum)

-- | The arity of a constant, /i.e./ how many arguments it expects.
--   The runtime system will collect arguments to a constant (see
--   'Swarm.Language.Value.VCApp') until it has enough, then dispatch
--   the constant's behavior.
arity :: Const -> Int
arity c = case constMeta $ constInfo c of
  ConstMUnOp {} -> 1
  ConstMBinOp {} -> 2
  ConstMFunc a _ -> a

-- | Whether a constant represents a /command/.  Constants which are
--   not commands are /functions/ which are interpreted as soon as
--   they are evaluated.  Commands, on the other hand, are not
--   interpreted until being /executed/, that is, when meeting an
--   'FExec' frame.  When evaluated, commands simply turn into a
--   'VCApp'.
isCmd :: Const -> Bool
isCmd c = case constMeta $ constInfo c of
  ConstMFunc _ cmd -> cmd
  _ -> False

-- | Function constants user can call with reserved words ('wait',...).
isUserFunc :: Const -> Bool
isUserFunc c = case constMeta $ constInfo c of
  ConstMFunc {} -> True
  _ -> False

-- | Whether the constant is an operator. Useful predicate for documentation.
isOperator :: Const -> Bool
isOperator c = case constMeta $ constInfo c of
  ConstMUnOp {} -> True
  ConstMBinOp {} -> True
  ConstMFunc {} -> False

-- | Whether the constant is a /function/ which is interpreted as soon
--   as it is evaluated, but *not* including operators.
--
-- Note: This is used for documentation purposes and complements 'isCmd'
-- and 'isOperator' in that exactly one will accept a given constant.
isBuiltinFunction :: Const -> Bool
isBuiltinFunction c = case constMeta $ constInfo c of
  ConstMFunc _ cmd -> not cmd
  _ -> False

-- | Whether the constant is a /tangible/ command, that has an
--   external effect on the world.  At most one tangible command may be
--   executed per tick.
isTangible :: Const -> Bool
isTangible c = case tangibility (constInfo c) of
  Tangible {} -> True
  _ -> False

-- | Whether the constant is a /long/ command, that is, a tangible
--   command which could require multiple ticks to execute.  Such
--   commands cannot be allowed in @atomic@ blocks.
isLong :: Const -> Bool
isLong c = case tangibility (constInfo c) of
  Tangible Long -> True
  _ -> False

-- | Information about constants used in parsing and pretty printing.
--
-- It would be more compact to represent the information by testing
-- whether the constants are in certain sets, but using pattern
-- matching gives us warning if we add more constants.
constInfo :: Const -> ConstInfo
constInfo c = case c of
  Wait ->
    command 0 long $
      shortDoc
        (Set.singleton $ Mutation $ RobotChange BehaviorChange)
        "Wait for a number of time steps."
  Noop ->
    command 0 Intangible . doc Set.empty "Do nothing." $
      [ "This is different than `Wait` in that it does not take up a time step."
      , "It is useful for commands like if, which requires you to provide both branches."
      , "Usually it is automatically inserted where needed, so you do not have to worry about it."
      ]
  Selfdestruct ->
    command 0 short
      . doc
        (Set.singleton $ Mutation $ RobotChange ExistenceChange)
        "Self-destruct a robot."
      $ [ "Useful to not clutter the world."
        , "This destroys the robot's inventory, so consider `salvage` as an alternative."
        ]
  Move ->
    command 0 short $
      shortDoc
        (Set.singleton $ Mutation $ RobotChange PositionChange)
        "Move forward one step."
  Backup ->
    command 0 short $
      shortDoc
        (Set.singleton $ Mutation $ RobotChange PositionChange)
        "Move backward one step."
  Volume ->
    command 1 short
      . doc
        (Set.singleton $ Query $ Sensing EntitySensing)
        "Measure enclosed volume."
      $ [ "Specify the max volume to check for."
        , "Returns either the measured volume bounded by \"unwalkable\" cells,"
        , "or `unit` if the search exceeds the limit."
        , T.unwords
            [ "There is also an implicit hard-coded maximum of"
            , T.pack $ show globalMaxVolume
            ]
        ]
  Path ->
    command 2 short
      . doc
        (Set.singleton $ Query $ Sensing EntitySensing)
        "Obtain shortest path to the destination."
      $ [ "Optionally supply a distance limit as the first argument."
        , "Supply either a location (`inL`) or an entity (`inR`) as the second argument."
        , "If a path exists, returns the immediate direction to proceed along and the remaining distance."
        ]
  Push ->
    command 1 short
      . doc
        (Set.fromList [Mutation EntityChange, Mutation $ RobotChange PositionChange])
        "Push an entity forward one step."
      $ [ "Both entity and robot moves forward one step."
        , "Destination must not contain an entity."
        ]
  Stride ->
    command 1 short
      . doc
        (Set.singleton $ Mutation $ RobotChange PositionChange)
        "Move forward multiple steps."
      $ [ T.unwords ["Has a max range of", T.pack $ show maxStrideRange, "units."]
        ]
  Turn ->
    command 1 short $
      shortDoc
        (Set.singleton $ Mutation $ RobotChange PositionChange)
        "Turn in some direction."
  Grab ->
    command 0 short $
      shortDoc
        (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange])
        "Grab an item from the current location."
  Harvest ->
    command 0 short . doc (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange]) "Harvest an item from the current location." $
      [ "Leaves behind a growing seed if the harvested item is growable."
      , "Otherwise it works exactly like `grab`."
      ]
  Sow ->
    command 1 short . doc (Set.singleton $ Mutation EntityChange) "Plant a seed at current location" $
      [ "The entity this matures into may be something else."
      ]
  Ignite ->
    command 1 short
      . doc
        (Set.singleton $ Mutation EntityChange)
        "Ignite a combustible item in the specified direction."
      $ [ "Combustion persists for a random duration and may spread."
        ]
  Place ->
    command 1 short
      . doc
        (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange])
        "Place an item at the current location."
      $ ["The current location has to be empty for this to work."]
  Ping ->
    command 1 short
      . doc
        (Set.singleton $ Query $ Sensing RobotSensing)
        "Obtain the relative location of another robot."
      $ [ "The other robot must be within transmission range, accounting for antennas installed on either end, and the invoking robot must be oriented in a cardinal direction."
        , "The location (x, y) is given relative to one's current orientation:"
        , "Positive x value is to the right, negative left. Likewise, positive y value is forward, negative back."
        ]
  Give ->
    command 2 short $
      shortDoc
        (Set.singleton $ Mutation $ RobotChange InventoryChange)
        "Give an item to another actor nearby."
  Equip ->
    command 1 short $
      shortDoc
        (Set.fromList [Mutation $ RobotChange InventoryChange, Mutation $ RobotChange BehaviorChange])
        "Equip a device on oneself."
  Unequip ->
    command 1 short $
      shortDoc
        (Set.fromList [Mutation $ RobotChange InventoryChange, Mutation $ RobotChange BehaviorChange])
        "Unequip an equipped device, returning to inventory."
  Make ->
    command 1 long $
      shortDoc
        (Set.singleton $ Mutation $ RobotChange InventoryChange)
        "Make an item using a recipe."
  Has ->
    command 1 Intangible $
      shortDoc
        (Set.singleton $ Query $ Sensing RobotSensing)
        "Sense whether the robot has a given item in its inventory."
  Equipped ->
    command 1 Intangible $
      shortDoc
        (Set.singleton $ Query $ Sensing RobotSensing)
        "Sense whether the robot has a specific device equipped."
  Count ->
    command 1 Intangible $
      shortDoc
        (Set.singleton $ Query $ Sensing RobotSensing)
        "Get the count of a given item in a robot's inventory."
  Reprogram ->
    command 2 long
      . doc
        (Set.singleton $ Mutation $ RobotChange BehaviorChange)
        "Reprogram another robot with a new command."
      $ ["The other robot has to be nearby and idle."]
  Act ->
    command 1 long . doc (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange]) "Act upon an entity." $
      [ "This command transforms an entity in the world using a device."
      , "It will automatically `use` the appropriate device required by a recipe that takes the target entity as input."
      ]
  Drill ->
    command 1 long
      . doc
        (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange])
        "Drill through an entity."
      $ [ "Usually you want to `drill forward` when exploring to clear out obstacles."
        , "When you have found a source to drill, you can stand on it and `drill down`."
        , "See what recipes with drill you have available."
        , "The `drill` command may return the name of an entity added to your inventory."
        ]
  Use ->
    command 2 long . doc (Set.singleton $ Mutation EntityChange) "Use one entity upon another." $
      [ "Which entities you can `use` with others depends on the available recipes."
      , "The object being used must be a 'required' entity in a recipe."
      ]
  Build ->
    command 1 long . doc (Set.singleton $ Mutation $ RobotChange ExistenceChange) "Construct a new robot." $
      [ "You can specify a command for the robot to execute."
      , "If the command requires devices they will be taken from your inventory and "
          <> "equipped on the new robot."
      ]
  Salvage ->
    command 0 long . doc (Set.singleton $ Mutation $ RobotChange ExistenceChange) "Deconstruct an old robot." $
      ["Salvaging a robot will give you its inventory, equipped devices and log."]
  Say ->
    command 1 short . doc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Emit a message." $
      [ "The message will be in the robot's log (if it has one) and the global log."
      , "You can view the message that would be picked by `listen` from the global log "
          <> "in the messages panel, along with your own messages and logs."
      , "This means that to see messages from other robots you have to be able to listen for them, "
          <> "so once you have a listening device equipped messages will be added to your log."
      , "In creative mode, there is of course no such limitation."
      ]
  Listen ->
    command 1 long . doc (Set.singleton $ Query $ Sensing RobotSensing) "Listen for a message from other actors." $
      [ "It will take the first message said by the closest actor."
      , "You do not need to actively listen for the message to be logged though, "
          <> "that is done automatically once you have a listening device equipped."
      , "Note that you can see the messages either in your logger device or the message panel."
      ]
  Log -> command 1 Intangible $ shortDoc (Set.singleton $ Mutation LogEmission) "Log the string in the robot's logger."
  View ->
    command 1 short . doc (Set.singleton $ Query $ Sensing RobotSensing) "View the given actor." $
      [ "This will recenter the map on the target robot and allow its inventory and logs to be inspected."
      ]
  Appear ->
    command 2 short . doc (Set.singleton $ Mutation Cosmetic) "Set how the robot is displayed." $
      [ "You can either specify one character or five (one for each direction: down, north, east, south, west)."
      , "The default is \"X^>v<\"."
      , "The second argument is for optionally setting a display attribute (i.e. color)."
      ]
  Create ->
    command 1 short . doc (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange]) "Create an item out of thin air." $
      ["Only available in creative mode."]
  Halt -> command 1 short $ shortDoc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Tell a robot to halt."
  Time ->
    command 0 Intangible $
      shortDoc
        (Set.singleton $ Query $ Sensing WorldCondition)
        "Get the current time."
  Scout ->
    command 1 short . doc (Set.singleton $ Query $ Sensing RobotSensing) "Detect whether a robot is within line-of-sight in a direction." $
      [ "Perception is blocked by 'Opaque' entities."
      , T.unwords ["Has a max range of", T.pack $ show maxScoutRange, "units."]
      ]
  Whereami ->
    command 0 Intangible $
      shortDoc
        (Set.singleton $ Query $ Sensing RobotSensing)
        "Get the current x and y coordinates."
  LocateMe ->
    command 0 Intangible $
      shortDoc
        (Set.singleton $ Query $ Sensing RobotSensing)
        "Get the current subworld and x, y coordinates."
  Waypoint ->
    function 2 . doc (Set.singleton $ Query APriori) "Get the x, y coordinates of a waypoint, by name and index." $
      [ "Returns only waypoints in the same subworld as the calling robot."
      , "Wraps around modulo the number of waypoints with the given name.  Throws an exception if there are no waypoints with the given name."
      ]
  Waypoints ->
    function 1 . doc (Set.singleton $ Query APriori) "Get the list of x, y coordinates of a named waypoint" $
      [ "Returns only the waypoints in the same subworld as the calling robot."
      , "Since waypoint names can have plural multiplicity, returns a list of (x, y) coordinates)."
      ]
  Structures ->
    command 1 Intangible . doc (Set.singleton $ Query $ Sensing EntitySensing) "Get the x, y coordinates of the southwest corner of all constructed structures of a given name" $
      ["Since structures can have multiple occurrences, returns a list of (x, y) coordinates."]
  Floorplan ->
    command 1 Intangible . doc (Set.singleton $ Query APriori) "Get the dimensions of a structure template" $
      [ "Returns a tuple of (width, height) for the structure of the requested name."
      , "Yields an error if the supplied string is not the name of a structure."
      ]
  HasTag ->
    function 2 . doc (Set.singleton $ Query APriori) "Check whether the given entity has the given tag" $
      [ "Returns true if the first argument is an entity that is labeled by the tag in the second argument."
      , "Yields an error if the first argument is not a valid entity."
      ]
  TagMembers ->
    function 1 . doc (Set.singleton $ Query APriori) "Get the entities labeled by a tag." $
      [ "Returns a list of all entities with the given tag."
      , "The order of the list is determined by the definition sequence in the scenario file."
      ]
  Detect ->
    command 2 Intangible . doc (Set.singleton $ Query $ Sensing EntitySensing) "Detect an entity within a rectangle." $
      ["Locate the closest instance of a given entity within the rectangle specified by opposite corners, relative to the current location."]
  Resonate ->
    command 2 Intangible . doc (Set.singleton $ Query $ Sensing EntitySensing) "Count specific entities within a rectangle." $
      [ "Applies a strong magnetic field over a given area and stimulates the matter within, generating a non-directional radio signal. A receiver tuned to the resonant frequency of the target entity is able to measure its quantity."
      , "Counts the entities within the rectangle specified by opposite corners, relative to the current location."
      ]
  Density ->
    command 1 Intangible . doc (Set.singleton $ Query $ Sensing EntitySensing) "Count all entities within a rectangle." $
      [ "Applies a strong magnetic field over a given area and stimulates the matter within, generating a non-directional radio signal. A receiver measured the signal intensity to measure the quantity."
      , "Counts the entities within the rectangle specified by opposite corners, relative to the current location."
      ]
  Sniff ->
    command 1 short . doc (Set.singleton $ Query $ Sensing EntitySensing) "Determine distance to entity." $
      [ "Measures concentration of airborne particles to infer distance to a certain kind of entity."
      , "If none is detected, returns (-1)."
      , T.unwords ["Has a max range of", T.pack $ show maxSniffRange, "units."]
      ]
  Chirp ->
    command 1 short . doc (Set.singleton $ Query $ Sensing EntitySensing) "Determine direction to entity." $
      [ "Uses a directional sonic emitter and microphone tuned to the acoustic signature of a specific entity to determine its direction."
      , "Returns 'down' if out of range or the direction is indeterminate."
      , "Provides absolute directions if \"compass\" equipped, relative directions otherwise."
      , T.unwords ["Has a max range of", T.pack $ show maxSniffRange, "units."]
      ]
  Watch ->
    command 1 short . doc (Set.singleton $ Query $ Sensing EntitySensing) "Interrupt `wait` upon location changes." $
      [ "Place seismic detectors to alert upon entity changes to the specified location."
      , "Supply a direction, as with the `scan` command, to specify a nearby location."
      , "Can be invoked more than once until the next `wait` command, at which time the only the registered locations that are currently nearby are preserved."
      , "Any change to entities at the monitored locations will cause the robot to wake up before the `wait` timeout."
      ]
  Surveil ->
    command 1 Intangible $
      doc
        (Set.singleton $ Query $ Sensing EntitySensing)
        "Interrupt `wait` upon (remote) location changes."
        [ "Like `watch`, but instantaneous and with no restriction on distance."
        , "Supply absolute coordinates."
        ]
  Heading -> command 0 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Get the current heading."
  Blocked -> command 0 Intangible $ shortDoc (Set.singleton $ Query $ Sensing EntitySensing) "See if the robot can move forward."
  Scan ->
    command 1 Intangible . doc (Set.singleton $ Query $ Sensing EntitySensing) "Scan a nearby location for entities." $
      [ "Adds the entity (not actor) to your inventory with count 0 if there is any."
      , "If you can use sum types, you can also inspect the result directly."
      ]
  Upload -> command 1 short $ shortDoc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Upload a robot's known entities and log to another robot."
  Ishere -> command 1 Intangible $ shortDoc (Set.singleton $ Query $ Sensing EntitySensing) "See if a specific entity is in the current location."
  Isempty ->
    command 0 Intangible . doc (Set.singleton $ Query $ Sensing EntitySensing) "Check if the current location is empty." $
      [ "Detects whether or not the current location contains an entity."
      , "Does not detect robots or other actors."
      ]
  Self -> function 0 $ shortDoc (Set.singleton $ Query APriori) "Get a reference to the current robot."
  Parent -> function 0 $ shortDoc (Set.singleton $ Query APriori) "Get a reference to the robot's parent."
  Base -> function 0 $ shortDoc (Set.singleton $ Query APriori) "Get a reference to the base."
  Meet -> command 0 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Get a reference to a nearby actor, if there is one."
  MeetAll -> command 0 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Return a list of all the nearby actors."
  Whoami -> command 0 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Get the robot's display name."
  Setname -> command 1 short $ shortDoc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Set the robot's display name."
  Random ->
    command 1 Intangible . doc (Set.singleton $ Query PRNG) "Get a uniformly random integer." $
      ["The random integer will be chosen from the range 0 to n-1, exclusive of the argument."]
  Run -> command 1 long $ shortDoc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Run a program loaded from a file."
  Return -> command 1 Intangible $ shortDoc Set.empty "Make the value a result in `cmd`."
  Try -> command 2 Intangible $ shortDoc Set.empty "Execute a command, catching errors."
  Undefined -> function 0 $ shortDoc Set.empty "A value of any type, that is evaluated as error."
  Fail -> function 1 $ shortDoc Set.empty "A value of any type, that is evaluated as error with message."
  If ->
    function 3 . doc Set.empty "If-Then-Else function." $
      ["If the bool predicate is true then evaluate the first expression, otherwise the second."]
  Inl -> function 1 $ shortDoc Set.empty "Put the value into the left component of a sum type."
  Inr -> function 1 $ shortDoc Set.empty "Put the value into the right component of a sum type."
  Case -> function 3 $ shortDoc Set.empty "Evaluate one of the given functions on a value of sum type."
  Fst -> function 1 $ shortDoc Set.empty "Get the first value of a pair."
  Snd -> function 1 $ shortDoc Set.empty "Get the second value of a pair."
  Force -> function 1 $ shortDoc Set.empty "Force the evaluation of a delayed value."
  Not -> function 1 $ shortDoc Set.empty "Negate the boolean value."
  Neg -> unaryOp "-" 7 P $ shortDoc Set.empty "Negate the given integer value."
  Add -> binaryOp "+" 6 L $ shortDoc Set.empty "Add the given integer values."
  And -> binaryOp "&&" 3 R $ shortDoc Set.empty "Logical and (true if both values are true)."
  Or -> binaryOp "||" 2 R $ shortDoc Set.empty "Logical or (true if either value is true)."
  Sub -> binaryOp "-" 6 L $ shortDoc Set.empty "Subtract the given integer values."
  Mul -> binaryOp "*" 7 L $ shortDoc Set.empty "Multiply the given integer values."
  Div -> binaryOp "/" 7 L $ shortDoc Set.empty "Divide the left integer value by the right one, rounding down."
  Exp -> binaryOp "^" 8 R $ shortDoc Set.empty "Raise the left integer value to the power of the right one."
  Eq -> binaryOp "==" 4 N $ shortDoc Set.empty "Check that the left value is equal to the right one."
  Neq -> binaryOp "!=" 4 N $ shortDoc Set.empty "Check that the left value is not equal to the right one."
  Lt -> binaryOp "<" 4 N $ shortDoc Set.empty "Check that the left value is lesser than the right one."
  Gt -> binaryOp ">" 4 N $ shortDoc Set.empty "Check that the left value is greater than the right one."
  Leq -> binaryOp "<=" 4 N $ shortDoc Set.empty "Check that the left value is lesser or equal to the right one."
  Geq -> binaryOp ">=" 4 N $ shortDoc Set.empty "Check that the left value is greater or equal to the right one."
  Format -> function 1 $ shortDoc Set.empty "Turn an arbitrary value into a string."
  Read -> function 2 $ shortDoc Set.empty "Try to read a string into a value of the expected type."
  Print ->
    command 2 short
      . doc
        (Set.singleton $ Mutation $ RobotChange InventoryChange)
        "Print text onto an entity."
      $ [ "`print p txt` Consumes one printable `p` entity from your inventory, and produces an entity"
        , "whose name is concatenated with a colon and the given text."
        , "In conjunction with `format`, this can be used to print values onto entities such as `paper`{=entity}"
        , "and give them to other robots, which can reconstitute the values with `read`."
        ]
  Erase ->
    command 1 short
      . doc
        (Set.singleton $ Mutation $ RobotChange InventoryChange)
        "Erase an entity."
      $ [ "Consumes the named printable entity from your inventory, which must have something"
        , "printed on it, and produces an erased entity.  This can be used to undo"
        , "the effects of a `print` command."
        ]
  Concat -> binaryOp "++" 6 R $ shortDoc Set.empty "Concatenate the given strings."
  Chars -> function 1 $ shortDoc Set.empty "Counts the number of characters in the text."
  Split ->
    function 2 . doc Set.empty "Split the text into two at given position." $
      [ "To be more specific, the following holds for all `text` values `s1` and `s2`:"
      , "`(s1,s2) == split (chars s1) (s1 ++ s2)`"
      , "So split can be used to undo concatenation if you know the length of the original string."
      ]
  CharAt ->
    function 2 . doc Set.empty "Get the character at a given index." $
      [ "Gets the character (as an `int` representing a Unicode codepoint) at a specific index in a `text` value.  Valid indices are 0 through `chars t - 1`."
      , "Throws an exception if given an out-of-bounds index."
      ]
  ToChar ->
    function 1 . doc Set.empty "Create a singleton `text` value from the given character code." $
      [ "That is, `chars (toChar c) == 1` and `charAt 0 (toChar c) == c`."
      ]
  AppF ->
    binaryOp "$" 0 R . doc Set.empty "Apply the function on the left to the value on the right." $
      [ "This operator is useful to avoid nesting parentheses."
      , "For exaple:"
      , "`f $ g $ h x = f (g (h x))`"
      ]
  Swap ->
    command 1 short . doc (Set.fromList [Mutation EntityChange, Mutation $ RobotChange InventoryChange]) "Swap placed entity with one in inventory." $
      [ "This essentially works like atomic grab and place."
      , "Use this to avoid race conditions where more robots grab, scan or place in one location."
      ]
  Atomic ->
    command 1 Intangible . doc (Set.singleton MetaEffect) "Execute a block of commands atomically." $
      [ "When executing `atomic c`, a robot will not be interrupted, that is, no other robots will execute any commands while the robot is executing @c@."
      ]
  Instant ->
    command 1 Intangible . doc (Set.singleton MetaEffect) "Execute a block of commands instantly." $
      [ "Like `atomic`, but with no restriction on program size."
      ]
  Key ->
    function 1 . doc Set.empty "Create a key value from a text description." $
      [ "The key description can optionally start with modifiers like 'C-', 'M-', 'A-', or 'S-', followed by either a regular key, or a special key name like 'Down' or 'End'"
      , "For example, 'M-C-x', 'Down', or 'S-4'."
      , "Which key combinations are actually possible to type may vary by keyboard and terminal program."
      ]
  InstallKeyHandler ->
    command 2 Intangible . doc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Install a keyboard input handler." $
      [ "The first argument is a hint line that will be displayed when the input handler is active."
      , "The second argument is a function to handle keyboard inputs."
      ]
  Teleport -> command 2 short $ shortDoc (Set.singleton $ Mutation $ RobotChange PositionChange) "Teleport a robot to the given location."
  Warp -> command 2 short $ shortDoc (Set.singleton $ Mutation $ RobotChange PositionChange) "Relocate a robot to the given cosmic location."
  As -> command 2 Intangible $ shortDoc (Set.singleton $ Mutation $ RobotChange BehaviorChange) "Hypothetically run a command as if you were another robot."
  RobotNamed -> command 1 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Find an actor by name."
  RobotNumbered -> command 1 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Find an actor by number."
  Knows -> command 1 Intangible $ shortDoc (Set.singleton $ Query $ Sensing RobotSensing) "Check if the robot knows about an entity."
 where
  doc e b ls = ConstDoc e b (T.unlines ls)
  shortDoc e b = ConstDoc e b ""
  unaryOp s p side d =
    ConstInfo
      { syntax = s
      , fixity = p
      , constMeta = ConstMUnOp side
      , constDoc = d
      , tangibility = Intangible
      }
  binaryOp s p side d =
    ConstInfo
      { syntax = s
      , fixity = p
      , constMeta = ConstMBinOp side
      , constDoc = d
      , tangibility = Intangible
      }
  command a f d =
    ConstInfo
      { syntax = lowShow c
      , fixity = 11
      , constMeta = ConstMFunc a True
      , constDoc = d
      , tangibility = f
      }
  function a d =
    ConstInfo
      { syntax = lowShow c
      , fixity = 11
      , constMeta = ConstMFunc a False
      , constDoc = d
      , tangibility = Intangible
      }

  lowShow :: Show a => a -> Text
  lowShow = toLower . showT

-- | Maximum perception distance for
-- 'Chirp' and 'Sniff' commands
maxSniffRange :: Int32
maxSniffRange = 256

maxScoutRange :: Int
maxScoutRange = 64

maxStrideRange :: Int
maxStrideRange = 64

maxPathRange :: Integer
maxPathRange = 128

-- | Checked upon invocation of the command,
-- before flood fill computation, to ensure
-- the search has a reasonable bound.
--
-- The user is warned in the failure message
-- that there exists a global limit.
globalMaxVolume :: Integer
globalMaxVolume = 64 * 64
