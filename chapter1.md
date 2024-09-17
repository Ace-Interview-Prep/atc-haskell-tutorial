# Chapter 1: Types & Domain Theory

1. [Introduction to Types](#introduction)
2. [Simple Types and Type Signatures](#typesignatures)
3. [Domain-Driven Design](#ddd)
4. [Pairing Domain Theory with Type Theory](#typetheory)
5. [Advanced Type Concepts](#advancedtypes)
6. [Recap & Exercises](#recap)

## Introduction to Types <a name="introduction"></a>

### What Are Types?

Types are fundamental to understanding and writing robust code in Haskell. They are not just a way to categorize data but are crucial to how we model the real world within our applications. The closer our types mirror the real world, the easier our applications become to reason about, and the more robust they become. Let’s dive deeper into what types really are:

- **Types as Categorization**: Types categorize data into labelled sets with one or more variations. For instance, a `Bool` type categorizes data as either `True` or `False`, while a `String` type categorizes sequences of characters.
  
- **Types Represent Information**: They represent the kinds of information our application processes. For example, an `Integer` type represents whole numbers, which might be used to count items or represent IDs.

- **Types in Functions**: In Haskell, types are used in functions to ensure that functions receive and return the correct kinds of data. This is crucial because it allows us to predict and enforce the behavior of our functions. Unlike in some other languages, Haskell functions always return a value and cannot mutate state directly. This means that the internal workings of functions must also be correct, as the types will enforce valid input and output.

### Core Type Declarations in Haskell

Before we delve into function examples, let's explore the basic building blocks of Haskell types: data declarations, type declarations, and newtype declarations.

#### 1. **Data Declarations**
   - **Purpose**: A `data` declaration is used to define a new algebraic data type. This is the most common way to define new types in Haskell.
   - **Syntax**: 

     ```haskell
     data Color = Red | Green | Blue
     ```

     Here, `Color` is a new type with three possible values: `Red`, `Green`, and `Blue`. These values are called constructors. The `data` keyword allows us to define a type that can have multiple forms.

#### 2. **Type Declarations**
   - **Purpose**: A `type` declaration creates a type synonym, which is essentially an alias for an existing type. It doesn't create a new type but makes the code more readable.
   - **Syntax**:

     ```haskell
     type Name = String
     ```

     In this example, `Name` is a type synonym for `String`. Everywhere you see `Name` in the code, it is just a `String` underneath, but using `Name` can make your code more descriptive.

#### 3. **Newtype Declarations**
   - **Purpose**: The `newtype` declaration creates a new type that is distinct from its underlying type but has the same runtime representation. It’s often used for type safety without runtime overhead.
   - **Syntax**:

     ```haskell
     newtype CustomerId = CustomerId Int
     ```

     Here, `CustomerId` is a new type that wraps an `Int`. While it’s just an `Int` at runtime, the type system treats `CustomerId` and `Int` as distinct, preventing you from accidentally mixing them up.

#### 4. **Function Definitions**
   - **Purpose**: Functions in Haskell are first-class citizens, meaning they can be passed as arguments, returned from other functions, and assigned to variables. Functions also have types, and these types are crucial in ensuring that functions interact with data correctly.
   - **Syntax**:

     ```haskell
     greet :: Name -> String
     greet name = "Hello, " ++ name ++ "!"
     ```

     In this example, `greet` is a function that takes a `Name` (which is a synonym for `String`) and returns a `String`. The type signature `Name -> String` ensures that `greet` only accepts a `Name` as input and will always produce a `String` as output.

### How Are Types in FP Different?

In Haskell, types are first-class citizens. This means that they are treated as values by the compiler, allowing you to encode complex logic directly into your types. This capability leads to predictable and safe code because the types themselves enforce the rules of your application.

**Comparison with Other Languages:**
- In languages like C#, types are often tied to classes and objects, and while types are important, they don’t offer the same level of expressiveness and safety. In C#, it’s possible to misuse types, especially when dealing with inheritance and mutable state, leading to unpredictable behavior deep in the execution of a program.
- Haskell’s type system, based on the Hindley-Milner type system, is much more powerful. It supports features like type inference, algebraic data types (ADTs), and pattern matching, all of which allow you to write concise yet expressive code without sacrificing safety.

### Why Types Matter

Having strict and well-thought-out types is crucial to robust system design. Let’s break down why:

- **Clarity and Predictability**: Types provide a clear contract for what a function does. When you look at a Haskell type signature, you can often understand what the function does without even seeing the implementation. This clarity is invaluable, especially in large codebases where multiple developers are working together. It reduces the likelihood of bugs and makes the system easier to maintain.
  
- **Avoiding Runtime Errors**: In dynamically-typed or loosely-typed languages, many errors only appear at runtime, leading to potential crashes or unpredictable behavior. Haskell’s strong type system catches these errors at compile time, significantly reducing the risk of runtime failures.
  
- **Reasoning About Code**: In Haskell, you can often reason about what a piece of code does just by looking at its type signature. This allows you to find and apply functions that match the type signature you need, often without needing to write new code. Hoogle, a Haskell-specific search engine, lets you search for functions by their type signatures, making it incredibly easy to integrate the right function into your program.

### Example: Compiler Failures in Procedural Programming

Let’s explore some examples where Haskell’s type system helps prevent runtime errors that are common in procedural languages like C.

#### Procedural Example in C:

```c
#include <stdio.h>

int divide(int a, int b) {
    return a / b;
}

int main() {
    int result = divide(10, 0);
    printf("Result: %d\n", result);
    return 0;
}
```


Expected Error:

- When you run this C code, dividing by zero will lead to undefined behavior, which can cause a crash or other unexpected results.

Typical Error Output:

- Depending on the system, you might see an error like “Floating point exception: division by zero” or the program might just crash without a clear error message.

Safe Haskell Equivalent:

```haskell
divide :: Integer -> Integer -> Maybe Integer
divide _ 0 = Nothing
divide a b = Just (a `div` b)

main :: IO ()
main = case divide 10 0 of
    Nothing -> putStrLn "Cannot divide by zero!"
    Just result -> putStrLn ("Result: " ++ show result)
```

Expected Result:

- This Haskell code safely handles the division by zero case, preventing a runtime error. Instead of crashing, it prints a friendly message: “Cannot divide by zero!”

Explanation:

- The use of Maybe in the type signature indicates that the result of divide might be Nothing (indicating an invalid operation) or Just a value (indicating a valid division). This forces the programmer to handle both cases explicitly, preventing runtime surprises.

#### The Magic of the Hindley-Milner Type System

One of the things that makes Haskell’s type system so powerful is the Hindley-Milner type system, which supports type inference and currying:

- Type Inference: The compiler can often deduce the types of expressions without explicit annotations. This reduces boilerplate and lets you write concise code while still enjoying the safety of static typing.
- Currying: In Haskell, every function is curried by default, meaning that functions with multiple arguments are treated as a series of functions that each take a single argument. This allows for partial application, where a function is applied to some of its arguments, returning another function that takes the remaining arguments. The Hindley-Milner type system ensures that even partially applied functions are type-safe at every step.

#### Example of Currying in Haskell:

```haskell
add :: Integer -> Integer -> Integer
add x y = x + y

increment :: Integer -> Integer
increment = add 1
```

Here, add is a curried function that takes two Integer arguments. By partially applying add with the value 1, we create a new function increment that only takes a single argument and adds 1 to it. The type system ensures that this transformation is valid and safe.

## Simple Types and Type Signatures <a name="typesignatures"></a>

### How to Read Type Signatures

Type signatures are an essential part of Haskell programming. They describe the types of the inputs a function takes and the type of output it produces. Understanding how to read and interpret these signatures is critical to writing and maintaining robust Haskell code.

- **Basic Type Signature**: A typical type signature in Haskell looks like this:

  ```haskell
  functionName :: InputType1 -> InputType2 -> OutputType
  ```

This means that the function functionName takes two inputs: one of type InputType1 and one of type InputType2, and it returns an output of type OutputType.

Example:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

In this example, add takes two integers (Int -> Int) and returns another integer (Int).

- **Complex Type Signatures**: Type signatures can get more complex, especially in larger Haskell libraries like Obelisk or Reflex, where cross-platform mobile development is involved.

Example:

```haskell
widgetHold :: forall t m a. (MonadHold t m, PostBuild t m) => m a -> Event t (m a) -> m (Dynamic t a)
```

Let’s break this down:
- `forall t m a.`: This means the function is polymorphic over types t, m, and a.
- `(MonadHold t m, PostBuild t m) =>`: These are type class constraints, meaning m must be an instance of MonadHold and PostBuild.
- `m a -> Event t (m a) -> m (Dynamic t a)`: This is the core function signature. It takes an m a, an Event t (m a), and returns an m (Dynamic t a).
Even though this looks complicated, it’s fundamentally still the same concept of specifying the flow of types through the function.

- **Partial Application and Currying**: Haskell functions are curried by default. This means every function actually takes one argument and returns another function if more arguments are needed. It’s important to remember that partial application allows you to call a function with fewer arguments than it expects, returning a new function that waits for the remaining arguments.

Currying Example:

```haskell
add :: Int -> Int -> Int
add x y = x + y

increment :: Int -> Int
increment = add 1
```

In this example, add takes two integers, but by partially applying it with the argument 1, we create a new function increment that takes only one integer and adds 1 to it.

Haskell hides this complexity, but understanding currying is crucial when reading type signatures with multiple -> symbols. You can think of Int -> Int -> Int as Int -> (Int -> Int)—a function that returns another function.

Explicit Currying Example:
```haskell
addCurried :: Int -> Int -> Int
addCurried = \x -> \y -> x + y
```

This is equivalent to the earlier add function, but here you can explicitly see how addCurried takes one argument (x), and returns a function that takes the next argument (y), and then produces the sum of x and y.

- A Quick Note on forall: You might come across forall in complex type signatures like the one shown above. forall introduces type variables that can be replaced with any type. It’s often used in polymorphic functions where the types aren’t known in advance. By default, Haskell assumes forall is present in type signatures without it being explicitly written, so you don’t need to worry about it in most cases.

#### Primitive Types

Primitive types in Haskell are the basic building blocks for more complex types. They represent simple values like numbers, characters, booleans, etc. These types are fundamental to every Haskell program and allow you to represent data in the most basic forms.

#### Common Primitive Types

- Int: Represents a fixed-size integer. It’s faster but limited in range. Example: 42.
- Integer: Represents an arbitrary-precision integer, meaning it can handle very large numbers. Example: 12345678901234567890.
- Float: Represents a single-precision floating-point number. Example: 3.14.
- Double: Represents a double-precision floating-point number, which is more precise than Float. Example: 2.71828.
- Char: Represents a single character. Example: 'a'.
- Bool: Represents a boolean value, which can be either True or False. Example: True.
- String: Represents a sequence of characters (which is actually a list of Char values). Example: "Hello, world!".

#### Primitive Type Examples

```haskell
-- Integer
age :: Int
age = 30

-- Floating-point number
piValue :: Double
piValue = 3.14159

-- Boolean
isSunny :: Bool
isSunny = True

-- Character
initial :: Char
initial = 'H'

-- String
greeting :: String
greeting = "Hello, Haskell!"
```

These types are the building blocks for more complex data structures that you’ll define using algebraic data types (ADTs), which we’ll cover shortly.

#### Polymorphic Types

Polymorphic types allow functions to be more flexible by operating on any type. Polymorphic functions are functions that work with type variables, rather than specific types.

**Example of Polymorphic Types:**

```haskell
identity :: a -> a
identity x = x
```

In this example, identity is a polymorphic function. The type variable a can be replaced with any type. This function takes an input of type a and returns a value of the same type.

#### Type Class Constraints

You can restrict polymorphic types using type class constraints. This allows the polymorphic function to work only with types that implement a particular type class.

**Example with Type Class Constraints:**

```haskell
addValues :: Num a => a -> a -> a
addValues x y = x + y
```

Here, `Num a =>` is a type class constraint that restricts a to types that are instances of the Num type class (such as Int, Float, etc.). This ensures that addValues can only be used with numeric types.

### Algebraic Data Types (ADTs)

Algebraic Data Types (ADTs) allow you to define custom types by combining other types. ADTs are foundational to Haskell and functional programming, allowing you to model complex data structures in a way that’s easy to reason about.

#### Sum Types and Product Types

- **Sum Types**: Represent a choice between multiple alternatives. A value of a sum type can take one of several forms.

Example of a Sum Type:

```haskell
data Color = Red | Green | Blue
```

Here, Color is a sum type because a value of type Color can be either Red, Green, or Blue.

- **Product Types**: Combine multiple types into a single type. A value of a product type contains values for each of the constituent types.

Example of a Product Type:

```haskell
data Point = Point Int Int
```

Here, Point is a product type that contains two Int values representing the x and y coordinates of a point.

#### Defining and Using ADTs

You can use ADTs to model complex real-world data in your application.

Example:

```haskell
data Shape = Circle Float
           | Rectangle Float Float
           | Square Float
```

- Shape is an ADT that can be a Circle with a radius (Float), a Rectangle with a width and height (Float Float), or a Square with a side length (Float).

#### Pattern Matching with ADTs

Pattern matching allows you to deconstruct ADT values and apply different logic depending on the constructor used.

Example of Pattern Matching with ADTs:

```haskell
area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h
area (Square s) = s * s
```

In this example, the area function computes the area of a Shape. It uses pattern matching to determine whether the shape is a Circle, Rectangle, or Square, and calculates the area accordingly.

## Domain-Driven Design <a name="ddd"></a>

### What is Domain-Driven Design?

Domain-Driven Design (DDD) is a methodology for structuring software around a business domain. It’s all about creating a deep understanding of the business or system you’re building and then reflecting that understanding directly in your code. The goal is to model real-world concepts with code that mirrors the domain as closely as possible.

In essence:
- **Model Real-World Concepts**: In DDD, the focus is on modeling the real world as accurately as possible, avoiding the use of primitive types like `Int` and `String` except at the "leaf" levels (e.g., values or properties that directly map to basic concepts like age or names). Instead, we create types that describe domain-specific concepts like `CustomerId`, `OrderId`, or `RobotArmAngle`. These types are meaningful and self-explanatory in the domain.
  
- **Use Domain Language**: A key principle of DDD is using the same language that the client uses to describe their domain. This means that, when you’re building a system for a specific business or project, you use terms that make sense to the client. Ideally, your client should be able to understand your code—or at least be able to provide meaningful input into the design of your domain models. When you’ve done it right, your code should read like plain English to domain experts.

#### Example of Domain-Driven Design

Imagine you're tasked with building a home robot system, where one of its key functionalities is to interact with objects around the house. The robot needs to perform tasks like picking up objects, turning knobs, and pressing buttons. Your client, an engineer with expertise in robotics, describes the system using terms like “robot arm,” “actuator,” “gripper,” and “torque.”

If you model this system using primitives (`Int`, `String`, `Float`), your code might look something like this:

```haskell
pickUp :: Int -> Float -> String -> Bool
pickUp armId rotation gripper = -- perform task
```

This code uses generic types like Int, Float, and String, which have no meaningful connection to the domain concepts. The client would have a hard time understanding what this function does or whether it models the domain correctly.

Instead, with DDD, you would model the system using types that reflect the domain:

```haskell
data RobotArm = RobotArm ArmId Rotation Torque
data Gripper = Gripper GripperType GripStrength

pickUp :: RobotArm -> Gripper -> Object -> TaskResult
pickUp arm gripper object = -- perform task
```

Now the function pickUp uses domain-specific types (RobotArm, Gripper, Object, TaskResult), making it easier for your client to understand and ensuring that your code accurately models the real-world system. The types tell you a lot more about what’s going on, reducing ambiguity and making your system safer and easier to reason about.

#### Modelling the Domain with Types

Let’s look at a more complex example to illustrate how types can model real-world interactions between different parts of a system.

Example Scenario: Robot Arm and Torso Integration

Imagine a robotics project where one team is responsible for building the robot’s arm, and another team is responsible for the torso. The two components need to be integrated at the shoulder joint. The arm has actuators that rotate at certain angles, and the torso needs to provide power to these actuators. Both teams are working independently, and the shoulder becomes the integration point for the two components.

Hypothetical Background:

- The Arm Team is working with actuators that rotate between 0 and 180 degrees.
- The Torso Team is responsible for delivering power, ensuring that the arm’s actuators receive enough power to function.
- The Shoulder acts as the integration point where the power and rotation capabilities meet.

We need to model the interaction between these systems, ensuring that the power delivered by the torso matches the requirements of the actuators in the arm.

Example Code:


```haskell
-- Define the arm's actuators and rotation
data Rotation = Rotation Int -- value in degrees between 0 and 180
data Actuator = Actuator Rotation Torque

-- Define the torso's power capabilities
data PowerSupply = PowerSupply Torque -- Torque provided to the actuators

-- Define the shoulder as the integration point
data Shoulder = Shoulder Actuator PowerSupply

-- Function to match the power provided by the torso with the actuator's requirements
integrateShoulder :: Shoulder -> Bool
integrateShoulder (Shoulder (Actuator (Rotation angle) torque) (PowerSupply supplyTorque))
    | torque <= supplyTorque = True -- Power matches or exceeds the actuator's requirement
    | otherwise = False -- Insufficient power
```

In this example:

- We’ve modeled Rotation, Actuator, and PowerSupply using Haskell’s type system to reflect the real-world domain.
- The function integrateShoulder checks whether the power supplied by the torso is sufficient for the actuators in the arm to function properly.

This domain model allows us to precisely describe the real-world system, ensuring that the interaction between the arm and torso is modeled correctly. By using meaningful types, we avoid the pitfalls of using primitives like Int and Float, which don’t convey the intent or constraints of the system.

#### Defining Domain Concepts as Types

Haskell’s type system is incredibly powerful and can be used to model much more than just basic data structures. You can define complex concepts using types, allowing you to encode domain-specific logic directly into the type system.

Example: Aggregating Data from Space Agencies

Let’s say we’re building a system that aggregates real-time data from three different space agencies. Each agency provides slightly different data formats, and some of the data might be missing or delayed. To ensure reliability, we need to aggregate the data and serve the most accurate, up-to-date information to the end user. We’ll validate the data using a round-robin algorithm to determine which data is most trustworthy.

Complex Domain Model:

```haskell
-- Define a type for data from Space Agencies
data SpaceAgency = NASA | ESA | JAXA

-- Define a type for the data each agency provides
data SpaceData = SpaceData {
    temperature :: Maybe Float,
    pressure    :: Maybe Float,
    velocity    :: Maybe Float
}

-- A type class to validate data from each agency
class ValidateData a where
    validate :: a -> Bool

instance ValidateData SpaceData where
    validate (SpaceData temp press vel) = all isJust [temp, press, vel]

-- Define a type for the round-robin algorithm to choose the most accurate data
data RoundRobin = RoundRobin SpaceData SpaceData SpaceData

-- Function to aggregate data from multiple agencies and serve the best one
aggregateData :: RoundRobin -> SpaceData
aggregateData (RoundRobin nasa esa jaxa)
    | validate nasa = nasa
    | validate esa  = esa
    | validate jaxa = jaxa
    | otherwise     = error "All data invalid"

-- Example usage
nasaData = SpaceData (Just 20.5) (Just 1013.25) Nothing
esaData = SpaceData (Just 20.4) (Just 1013.30) (Just 7500)
jaxaData = SpaceData Nothing (Just 1013.20) (Just 7501)

main = print $ aggregateData (RoundRobin nasaData esaData jaxaData)
```

In this example:

- We define the SpaceData type, which represents the real-time data from each space agency. Some fields may be missing (Maybe Float), which we handle explicitly.
- The ValidateData type class allows us to check whether the data provided by each agency is complete and valid.
- The RoundRobin type models the round-robin algorithm used to select the most reliable data.
- The aggregateData function aggregates data from NASA, ESA, and JAXA, and returns the most accurate dataset based on the validation criteria.


## Pairing Domain Theory with Type Theory <a name="typetheory"></a>

### Type Theory in the Context of Domain Modeling

In Haskell, type theory goes hand in hand with domain-driven design. Domain theory helps you model the real world by representing domain concepts in code, while type theory ensures that your code is precise, safe, and free from common runtime errors. The power of Haskell’s type system lies in its ability to **perfectly describe your problem space**.

#### How Types Make Domains Safer

When you define a domain model, you're not just giving names to concepts—you're also giving those concepts constraints and rules through types. For example:
- **Simple types like `Int` or `String`** don’t carry much meaning by themselves. They could be anything.
- **Custom types like `CustomerId` or `Temperature`** represent specific concepts in your domain and ensure that values are used in appropriate contexts. By encoding your domain logic directly into types, you prevent mistakes like accidentally passing an email address where a `CustomerId` is expected.

Let’s break this down with a simple example.

#### Example: Defining Safe Domain Concepts

Consider an online shopping platform. You’ll have concepts like customers, orders, and products. You could model these using primitives like `Int` and `String`, but this opens the door to mistakes:

```haskell
-- This is unsafe
createOrder :: Int -> String -> String -> Int -> Bool
createOrder customerId productName address quantity = -- Implementation
```

In this version, we’re using Int and String everywhere, which doesn’t reflect the domain at all. It’s easy to accidentally swap parameters or use invalid values.

With type theory, we can give more meaning and safety to the domain:

```haskell
-- Safe version using custom types
newtype CustomerId = CustomerId Int
newtype ProductName = ProductName String
newtype Address = Address String
newtype Quantity = Quantity Int

createOrder :: CustomerId -> ProductName -> Address -> Quantity -> Bool
createOrder (CustomerId custId) (ProductName prodName) (Address addr) (Quantity qty) = -- Implementation
```

By defining types like CustomerId, ProductName, Address, and Quantity, we give more meaning to our code. The type signature for createOrder now clearly reflects what the function does, and we prevent mistakes like passing an address where a ProductName is expected.

### Ensuring Valid States with Types

One of the most powerful aspects of Haskell’s type system is that it allows you to enforce valid states directly in the type system. This means you can encode business rules or constraints into your types, making it impossible to represent invalid states.

#### Example: Preventing Invalid Orders

Let’s extend our Order example. In a typical e-commerce application, an order should never have a negative quantity, and it should always have a valid product. We can enforce these rules in the type system.

Basic Example (with risk of invalid states):

```haskell
data Order = Order Int String Int -- CustomerId, ProductName, Quantity
```

In this version, there’s nothing stopping us from creating an Order with an invalid product name or a negative quantity.

Improved Example (enforcing valid states):

```haskell
newtype CustomerId = CustomerId Int
newtype ProductName = ProductName String
newtype Address = Address String
newtype Quantity = Quantity Int

data ValidatedOrder = ValidatedOrder CustomerId ProductName Address Quantity
```

But we can go even further to ensure that only valid orders can exist:

```haskell
newtype NonEmptyString = NonEmptyString String
    deriving (Show)

mkNonEmptyString :: String -> Maybe NonEmptyString
mkNonEmptyString "" = Nothing
mkNonEmptyString str = Just (NonEmptyString str)

newtype PositiveInt = PositiveInt Int
    deriving (Show)

mkPositiveInt :: Int -> Maybe PositiveInt
mkPositiveInt n
    | n > 0     = Just (PositiveInt n)
    | otherwise = Nothing

data Order = Order {
    customerId :: CustomerId,
    productName :: NonEmptyString,
    address :: NonEmptyString,
    quantity :: PositiveInt
}

createOrder :: CustomerId -> String -> String -> Int -> Maybe Order
createOrder cid pName addr qty = do
    pName' <- mkNonEmptyString pName
    addr'  <- mkNonEmptyString addr
    qty'   <- mkPositiveInt qty
    return $ Order cid pName' addr' qty'
```


In this version:

- NonEmptyString ensures that product names and addresses can never be empty.
- PositiveInt ensures that quantities are always positive.

The createOrder function now returns a Maybe Order, meaning that it will only produce a valid Order if all the inputs meet the necessary conditions. This way, you can guarantee at compile time that no invalid orders can be created.

### How the Compiler Infers Types

Haskell’s compiler is powerful enough to infer types even if you don’t explicitly annotate them. However, it’s always a good idea to include type signatures for clarity and better error checking.

### Example of Type Inference:

```haskell
add :: Int -> Int -> Int
add x y = x + y
```

Even if you omit the type signature, Haskell can infer that add takes two Ints and returns an Int:

```haskell
add x y = x + y -- Haskell infers: add :: Int -> Int -> Int
```

### Type Inference with Polymorphism:

Haskell can also infer types for polymorphic functions. For example:

```haskell
identity x = x
```

Haskell infers that identity can work with any type, so the type signature becomes:

```haskell
identity :: a -> a
```

In this case, a is a type variable, meaning that identity can take and return any type.

#### Practical Example: Modelling a Bank Account System

To see how type theory and domain modeling work together in a real-world scenario, let’s model a banking system.

1. Domain Concepts:
- Account: Represents a bank account.
- Transaction: Represents a deposit or withdrawal.
- Balance: Represents the current balance of the account.
2. Domain-Specific Types:
- We’ll define custom types for these concepts to ensure that invalid states (e.g., negative balances, invalid transactions) are impossible.

Example Code:

```haskell
newtype AccountId = AccountId Int
newtype Amount = Amount Float
    deriving (Show)

data TransactionType = Deposit | Withdrawal
    deriving (Show)

data Transaction = Transaction {
    transactionType :: TransactionType,
    amount :: Amount
} deriving (Show)

data Account = Account {
    accountId :: AccountId,
    balance   :: Amount
} deriving (Show)

-- Function to apply a transaction to an account
applyTransaction :: Account -> Transaction -> Maybe Account
applyTransaction (Account accId (Amount bal)) (Transaction Deposit (Amount amt)) =
    Just (Account accId (Amount (bal + amt)))

applyTransaction (Account accId (Amount bal)) (Transaction Withdrawal (Amount amt))
    | bal >= amt = Just (Account accId (Amount (bal - amt)))
    | otherwise  = Nothing -- Prevent overdraft

-- Example usage
main = do
    let account = Account (AccountId 1) (Amount 1000)
    let deposit = Transaction Deposit (Amount 200)
    let withdrawal = Transaction Withdrawal (Amount 1500)

    print $ applyTransaction account deposit      -- Valid deposit
    print $ applyTransaction account withdrawal   -- Invalid withdrawal (overdraft)
```

In this example:

- AccountId, Amount, and Transaction are modeled using custom types.
- The applyTransaction function ensures that withdrawals can only happen if there’s enough balance, preventing overdrafts.
- This ensures that invalid states (e.g., negative balances) are impossible to represent.


## Advanced Type Concepts <a name="advancedtypes"></a>

In this section, we’ll introduce some of Haskell’s more advanced type concepts, such as type classes, type families, phantom types, and type-level programming. You don’t need to learn these concepts right away, but knowing how to recognize them and having a basic understanding of how they work will help you expand your knowledge over time.

### Type Classes and Overloading

**Type classes** are one of the most powerful features in Haskell. They provide a way to define generic interfaces that can be implemented by different types. Type classes allow for **ad hoc polymorphism**, meaning that you can write functions that work with any type as long as that type implements certain behavior (i.e., is an instance of a type class).

Think of type classes like interfaces in object-oriented programming. However, type classes are more flexible and allow Haskell’s type system to express a wide range of concepts while ensuring type safety.

#### Example: The `Eq` Type Class

The `Eq` type class defines an interface for equality testing. If a type is an instance of `Eq`, you can use the `(==)` and `(/=)` operators to compare values of that type.

```haskell
class Eq a where
    (==) :: a -> a -> Bool
    (/=) :: a -> a -> Bool

-- Making a custom data type an instance of Eq
data Color = Red | Green | Blue

instance Eq Color where
    Red == Red     = True
    Green == Green = True
    Blue == Blue   = True
    _ == _         = False

-- Example usage:
isEqual :: Color -> Color -> Bool
isEqual Red Green = Red == Green -- False
```

In this example:

- We define the Color data type with three possible values: Red, Green, and Blue.
- We make Color an instance of the Eq type class by defining how equality works for the Color type.
- Once we’ve defined this, we can compare Color values using the == operator.

### Overloading Functions with Type Classes

Type classes also allow us to overload functions. A function can behave differently depending on the type of the arguments it receives, as long as the type is an instance of the relevant type class.

For example, the + operator is overloaded by the Num type class, which defines numeric operations:

```haskell
class Num a where
    (+) :: a -> a -> a
    -- other numeric operations...
```

This means that + can be used with any type that’s an instance of Num (like Int, Float, etc.).

### Polymorphism with Type Classes

You can define polymorphic functions that work with any type that’s an instance of a specific type class. Here’s an example of a polymorphic function that works with any type that implements the Show type class (which is responsible for converting values to strings):

```haskell
printValue :: Show a => a -> String
printValue value = "The value is: " ++ show value
```

Here, Show a => is a type class constraint. It means that printValue can accept any type a as long as a is an instance of the Show type class. The function then uses the show function (which converts a value to a String) to display the value.

### Type Families

Type families are a more advanced feature that allows you to associate types with type classes, effectively creating a form of type-level functions. They enable more flexibility and power in type classes, allowing for more dynamic behavior based on types.

#### Example: Type Families for Different Container Types

Let’s say we want to define a type class for container-like data structures (e.g., List, Maybe, etc.), but we want to allow different types of elements in these containers. Type families can help us here.

```haskell
{-# LANGUAGE TypeFamilies #-}

class Container c where
    type Element c
    empty :: c
    insert :: Element c -> c -> c

-- List instance of Container
instance Container [a] where
    type Element [a] = a
    empty = []
    insert x xs = x : xs

-- Maybe instance of Container
instance Container (Maybe a) where
    type Element (Maybe a) = a
    empty = Nothing
    insert x _ = Just x
```

In this example:

- We define a Container type class with a type family Element c, which represents the type of elements that can be stored in the container.
- We then provide two instances: one for lists ([a]) and one for Maybe a. Each instance defines what kind of elements can be inserted and how to handle insertion and the “empty” state.

### Phantom Types for Extra Type Safety

Phantom types are an advanced feature in Haskell where a type parameter is included in the type definition but is not used in the actual data. They are useful for adding extra type safety to your programs, especially when you want to encode additional constraints or metadata into the types.

#### Example: Phantom Types for Unit Safety

Let’s say we want to create a system that tracks distances, but we want to make sure that we never accidentally mix up meters and kilometers.

```haskell
{-# LANGUAGE GADTs #-}

data Meters
data Kilometers

data Distance a where
    MkDistance :: Double -> Distance a

convertToKilometers :: Distance Meters -> Distance Kilometers
convertToKilometers (MkDistance d) = MkDistance (d / 1000)

-- Example usage
distanceInMeters :: Distance Meters
distanceInMeters = MkDistance 5000

distanceInKilometers :: Distance Kilometers
distanceInKilometers = convertToKilometers distanceInMeters
```

In this example:

- We define two phantom types, Meters and Kilometers, which represent the units of distance.
- The Distance a type takes a phantom type a, which is either Meters or Kilometers. This ensures that distances are always associated with a specific unit.
- We define a convertToKilometers function that safely converts a distance in meters to kilometers.

With phantom types, you can ensure that your code is type-safe at compile time, preventing errors like mixing up units of measurement.

### Type-Level Programming

Haskell allows for type-level programming, where types can be manipulated much like values. This opens the door to highly expressive and type-safe code. You can perform computations and enforce constraints at the type level, reducing the need for runtime checks.

#### Example: Type-Level Natural Numbers

We can use type-level programming to represent physical dimensions (e.g., length, mass, time) and ensure that our units are consistent in mathematical operations.

```haskell
{-# LANGUAGE DataKinds #-}
{-# LANGUAGE KindSignatures #-}
{-# LANGUAGE TypeOperators #-}

data Meter
data Second

data Quantity (unit :: *) where
    MkQuantity :: Double -> Quantity unit

-- Adding two quantities of the same unit
addQuantities :: Quantity unit -> Quantity unit -> Quantity unit
addQuantities (MkQuantity x) (MkQuantity y) = MkQuantity (x + y)

-- Example usage
distance :: Quantity Meter
distance = MkQuantity 100

time :: Quantity Second
time = MkQuantity 9.58

-- This works:
sumDistance = addQuantities distance distance

-- This would fail to compile if uncommented (mismatched units):
-- sumTimeDistance = addQuantities distance time
```

In this example:

- We define a Quantity type that takes a unit (e.g., Meter or Second) as a type parameter.
- The addQuantities function ensures that we can only add quantities of the same unit.
- If we try to add a distance to a time, Haskell’s type system will catch this error at compile time.

## Recap & Exercises <a name="recap"></a>

### Recap

In this lesson, we’ve explored the foundational concepts of types and domain theory in Haskell. By understanding how types work in Haskell and how to apply domain-driven design, you’re now equipped with the tools to model real-world domains accurately and safely. Let’s review the key takeaways from this lesson:

- **Simple Types and Type Signatures**:
    - Type signatures describe the inputs and outputs of functions, providing a clear contract for what a function does.
    - Primitive types like `Int`, `Float`, `Bool`, and `String` are the basic building blocks for more complex data structures.
    - Haskell’s type system is based on currying, meaning that every function takes one argument and returns a new function if more arguments are required.
    - Polymorphic types allow functions to work with any type, providing flexibility and reuse.

- **Domain-Driven Design**:
    - Domain-driven design (DDD) is a methodology for structuring software around real-world business domains, using types to model the domain in code.
    - Avoid using primitive types like `Int` or `String` for domain concepts. Instead, define domain-specific types like `CustomerId`, `OrderId`, or `ProductName` to make your code more meaningful and safe.
    - Use the language of the business domain when modeling in code, ensuring that the types and structures reflect real-world concepts accurately.

- **Pairing Domain Theory with Type Theory**:
    - Haskell’s type system allows you to enforce valid states in your domain model by encoding business rules directly into the type system.
    - Type safety prevents invalid states, such as negative quantities or empty product names, from existing in your system, reducing runtime errors.
    - The Haskell compiler works with you to infer types, but explicit type annotations provide clarity and enforce constraints.

- **Advanced Type Concepts**:
    - **Type Classes**: Haskell’s type classes allow for ad hoc polymorphism, providing a way to define generic interfaces that can be implemented by different types.
    - **Type Families**: Type families let you associate types with type classes, allowing for more flexible and dynamic behavior based on types.
    - **Phantom Types**: Phantom types allow you to add extra type safety without affecting runtime behavior. They’re especially useful for encoding additional constraints or metadata into the type system.
    - **Type-Level Programming**: Haskell allows for type-level programming, enabling you to enforce constraints at the type level and reducing the need for runtime checks.

These are the building blocks of functional programming in Haskell and the key to creating robust, type-safe applications. By mastering types and domain modeling, you’re well on your way to becoming a proficient Haskell developer.

### Exercises

Now it’s time to put what you’ve learned into practice. The following exercises will help reinforce the concepts from this lesson by challenging you to think through real-world domain problems and apply Haskell’s type system effectively.

#### Exercise 1: Modeling a Banking System

**Objective**: Create a simple banking system using domain-driven design principles.

- Define the following types:
    - `AccountId`: A unique identifier for a bank account.
    - `Balance`: The current balance in the account (use a custom type to prevent negative balances).
    - `TransactionType`: An algebraic data type representing deposits and withdrawals.
    - `Transaction`: A record type representing a transaction (including the type and the amount).
- Write a function `applyTransaction :: Balance -> Transaction -> Maybe Balance` that updates the balance based on the transaction. Ensure that withdrawals cannot exceed the balance.

**Hint**: Use a custom type to ensure that `Balance` can never be negative.

```haskell
-- Example skeleton
newtype Balance = Balance Float
data TransactionType = Deposit | Withdrawal
data Transaction = Transaction { tType :: TransactionType, amount :: Float }

applyTransaction :: Balance -> Transaction -> Maybe Balance
-- Implement your function here
```

#### Exercise 2: Aggregating Data from Multiple Vendors

**Objective:** Model a system that aggregates data from three different vendors.

- Define a custom type Vendor with constructors VendorA, VendorB, and VendorC.
- Create a type DataFeed to represent real-time data from each vendor (e.g., temperature, humidity, and pressure).
- Write a function aggregateFeeds :: DataFeed -> DataFeed -> DataFeed -> DataFeed that combines the data from each vendor, selecting the most recent valid data for each field.

**Hint:** Use Maybe types for fields that might be missing, and use pattern matching to choose valid data.

```haskell
-- Example skeleton
data Vendor = VendorA | VendorB | VendorC
data DataFeed = DataFeed { temperature :: Maybe Float, humidity :: Maybe Float, pressure :: Maybe Float }

aggregateFeeds :: DataFeed -> DataFeed -> DataFeed -> DataFeed
-- Implement your function here
```

#### Exercise 3: Using Phantom Types for Unit Safety

**Objective:** Prevent unit mix-ups using phantom types.

- Define two phantom types Meters and Kilometers.
- Create a Distance type that uses phantom types to represent distances in meters or kilometers.
- Write a function convertToKilometers :: Distance Meters -> Distance Kilometers that converts a distance in meters to kilometers.
- Try to add distances in meters and kilometers together, and make sure the compiler prevents it!

```haskell
-- Example skeleton
data Meters
data Kilometers

data Distance a = MkDistance Double

convertToKilometers :: Distance Meters -> Distance Kilometers
-- Implement your function here
```

<br>
<br>
[Up Next: Chapter 2 - Immutability Changes Everything](./chapter2.md)
