# Chapter 2: Immutability Changes Everything

## Introduction: Currying, Pure Evaluation, and the Power of Immutability

Immutability is at the core of functional programming, and it fundamentally changes the way we think about writing code and how programs execute. In Haskell, this isn’t just a recommendation—it’s enforced. All data in Haskell is immutable by default, meaning once a value is assigned, it cannot be changed.

In contrast, object-oriented programming (OOP) tends to favor **mutable state**—where data can be modified directly. This seems convenient at first but introduces many hidden dangers. Mutable state can cause unpredictable behavior and bugs because data can change in ways you don’t expect.

### Currying and Pure Evaluation

One of the cornerstones of immutability in Haskell is the concept of **currying**. Every function in Haskell is curried, meaning it can be partially applied. You give it some arguments, and it returns a new function that takes the remaining arguments. This naturally leads to **pure evaluation**, where each step of a function is evaluated in isolation, without changing any state. This makes it easy to reason about your code since you know exactly what each part of the program does.

**Example:**

```haskell
add :: Int -> Int -> Int
add x y = x + y

-- Partial application of the curried function
increment = add 1
result = increment 5  -- Result is 6

Currying promotes immutability because the partially applied function is itself an immutable value. Once a function is curried, you can reuse it with different inputs without changing its behavior.

The Illusion of Mutability in the Real World

If you think about it, mutability doesn’t actually exist in the real world. The idea of mutating a variable, like x += 5, holds no real-world meaning. In the physical world, if you want to express that something changed, you would describe it as a new state rather than altering the previous one. For example, if you say, “I drank half of the water in the bottle,” you don’t modify the water. Instead, you observe a new state of the bottle—half-empty.

What we often refer to as state in programming is just a snapshot of some aspect of the system at a particular time. When you mutate a variable in a program, you’re not changing the “thing” itself but rather replacing it with something new, overwriting the old version in memory.

Metaphor of State Immutability: The Bucket Example

Think of a bucket that represents the state of your program. In a world of mutable state, you can throw things into the bucket, mix them up, and hand the bucket to someone else. But the problem is, the person you hand the bucket to doesn’t know what rules you’ve been following for mixing things. They have no idea what the original contents were or how you manipulated them.

On the other hand, in a world of immutable state, the bucket is passed around for people to observe, but no one is allowed to change its contents. Every time you need to modify the bucket’s contents, you create a new bucket that reflects the new state, while the original bucket stays the same. This way, everyone knows what’s in the bucket at every point in time, and there’s no ambiguity.

This metaphor captures the essence of immutability: you don’t change the state, you create a new state. This makes the flow of information in your program predictable, safer, and easier to reason about.

Why Immutability Changes Everything

Immutability might seem like a restriction at first, but ironically, it frees us from many of the problems that plague mutable systems. Here’s why immutability is such a game-changer:

	1.	No Hidden State: In mutable systems, state can change at any time, anywhere in the program, which makes it hard to reason about the behavior of your code. With immutability, the state of the program is always explicit. You always know what data is being worked with at any given time.
	2.	Easier Testing: Testing programs with mutable state often requires complex setups like mocks, dependency injection, and detailed tracking of side effects. In functional programming, where data is immutable and functions are pure, testing becomes much easier. You pass data into a function, observe the output, and know that the function doesn’t affect anything outside itself.
	3.	Concurrency and Parallelism: In a world with mutable state, concurrent programs often run into issues like race conditions or deadlocks, where two parts of the program are competing to change the same data at the same time. Immutability solves this problem by ensuring that no part of your program can ever change shared state, which means you can safely run operations in parallel without worrying about conflicting changes.
	4.	Predictable Code: In an immutable system, once a value is created, it can never change. This predictability eliminates many classes of bugs that are common in object-oriented systems, where data can change unexpectedly.

Object-Oriented Languages vs Functional Programming

In object-oriented languages, mutability is often the default behavior. Consider this Python example:

x = 5
x += 5  # Mutates the variable 'x', changing its value to 10

Here, the variable x is being mutated, which is a simple example, but when used in larger systems, this leads to a tangled web of state changes. In complex applications, you need to introduce mechanisms like:

	•	Mocks: Fake versions of objects used in tests because mutable dependencies are difficult to isolate.
	•	Dependency Injection: A way to inject dependencies at runtime to manage mutable state effectively.
	•	Extensive Unit Tests: Since any function could change state, you need extensive testing to ensure all potential side effects are covered.

In contrast, functional programming avoids these issues altogether. Since data is immutable and functions are pure, you don’t need mocks or dependency injection to isolate behavior. You can test each function in isolation without worrying about hidden state or side effects.

Examples of Immutability in Haskell

Let’s look at how Haskell handles immutability and avoids the problems of mutable state.

Immutable Data Structures

In Haskell, once you assign a value to a variable, it can never change. This ensures that your data is always consistent, and you don’t have to worry about it being changed unexpectedly.

-- Example of immutable variables
x :: Int
x = 5

-- Attempting to change 'x' will result in a compile-time error
-- x = x + 5 -- This is illegal in Haskell

In this code, x is assigned the value 5, and you cannot change x later. If you need a new value, you would create a new variable:

y :: Int
y = x + 5

Now, y contains the new value, but x remains untouched.

Pure Functions and State Changes

In Haskell, state changes are handled using pure functions, which means that a function’s output depends only on its input, and it doesn’t affect any external state. If you want to represent a change in state, you return a new version of the state, leaving the old state intact.

-- Pure function example
addFive :: Int -> Int
addFive x = x + 5

-- The input 'x' is unchanged; a new value is returned
result = addFive 10  -- result is 15

This pure function guarantees that the input x remains unchanged and a new value is returned.

Handling State with Monads

Haskell uses monads (such as the State monad) to manage state in a controlled and predictable way. Monads allow you to work with state changes while preserving immutability, ensuring that every state transition is explicit.

import Control.Monad.State

-- Define a stateful computation
incrementState :: State Int Int
incrementState = do
    n <- get
    put (n + 1)
    return n

-- Running the stateful computation
main = print $ runState incrementState 10

In this example:

	•	incrementState is a computation that works with state (an Int) and returns a new state.
	•	The state is threaded through the computation, but at no point is the original state mutated in place.

### Pure Functions and Referential Transparency

#### Pure Functions: The Core of Immutability

A **pure function** is a function that:
1. **Always produces the same output** given the same input.
2. **Has no side effects**, meaning it doesn't change any state or interact with the outside world (no printing, modifying global variables, etc.).

In a system that embraces immutability, pure functions are everywhere. They take inputs and produce outputs without altering anything in between. This creates code that is easy to reason about, predictable, and free from the hidden side effects that plague mutable systems.

**Example:**

```haskell
-- Pure function: No side effects, output depends only on the input
multiplyByTwo :: Int -> Int
multiplyByTwo x = x * 2

result = multiplyByTwo 10  -- result is 20

Here, multiplyByTwo is a pure function. It doesn’t rely on or modify any external state. It simply takes an integer, multiplies it by two, and returns the result. Every time you call multiplyByTwo 10, the result will always be 20. There’s no hidden state that could change the outcome.

Contrast this with an impure function from an object-oriented perspective:

int counter = 0;

public int incrementCounter() {
    return ++counter;
}

This incrementCounter function in Java is impure because it relies on the external mutable state (counter). Each time you call it, the result will change based on the current value of counter. This makes it much harder to predict how the program behaves at any given time, especially as the system grows in complexity.

Referential Transparency

One of the properties that follows directly from pure functions and immutability is referential transparency. A function or expression is referentially transparent if it can be replaced by its value without changing the behavior of the program.

Example of Referential Transparency:

-- Pure function
square :: Int -> Int
square x = x * x

-- The following expressions are referentially transparent
result1 = square 4  -- This expression can be replaced with its result: 16
result2 = 16  -- Both are equivalent

In a language that guarantees immutability like Haskell, referential transparency is always maintained. This is not true in mutable languages, where state can change unexpectedly, and replacing an expression with its value could yield different results based on the current state.

Why Does Referential Transparency Matter?

	•	Simplifies reasoning: You don’t need to track how the value of a variable might have changed elsewhere in the code. Each expression is self-contained and independent.
	•	Improves testability: You can test functions in isolation, knowing that they will always behave the same way given the same input.

Immutable Data Structures

In Haskell, all data structures are immutable by default. This means that once you create a data structure, it cannot be changed. If you want to “modify” it, you actually create a new data structure based on the old one.

Lists in Haskell: An Immutable Data Structure

One of the most fundamental data structures in Haskell is the list, which is immutable. If you want to add or remove elements from a list, you don’t modify the list in place. Instead, you create a new list.

Example:

-- Define a list
myList :: [Int]
myList = [1, 2, 3]

-- Add an element to the front of the list (creates a new list)
newList = 0 : myList  -- newList is [0, 1, 2, 3]

In this example, the : operator creates a new list by prepending the element 0 to the front of the existing list myList. The original list [1, 2, 3] remains unchanged.

Immutable Data with Records

Haskell also supports more complex data structures like records, which allow you to define immutable types with named fields.

Example:

-- Define a record for a User
data User = User { userId :: Int, userName :: String }

-- Create a User
user :: User
user = User { userId = 1, userName = "Alice" }

-- Create a new User with a different name
updatedUser = user { userName = "Bob" }

In this example:

	•	user is an immutable data structure representing a user.
	•	If you need to “update” the user’s name, you don’t modify the original user. Instead, you create a new updatedUser with the modified userName, while user remains unchanged.

Efficiency of Immutable Data Structures

At first glance, immutability might seem inefficient—if you’re creating new data structures all the time, won’t that slow things down? In reality, Haskell uses techniques like persistent data structures and lazy evaluation to optimize performance.

	•	Persistent Data Structures: Instead of copying the entire data structure every time, Haskell reuses parts of the old structure when possible. For example, if you add an element to the front of a list, Haskell doesn’t copy the entire list—it simply adds the new element and points to the existing list.
	•	Lazy Evaluation: Haskell only evaluates expressions when absolutely necessary. This means that data isn’t copied or created until it’s needed, reducing unnecessary computations and memory usage.

Concurrency and Immutability

Immutability has a profound effect on concurrency. In a mutable system, concurrency can lead to serious problems like race conditions and deadlocks when multiple threads try to access and modify the same state at the same time. This makes concurrent programming difficult and error-prone.

With immutability, these problems simply disappear. Since state cannot change, you don’t have to worry about threads interfering with each other. You can safely share data between threads without needing locks or other synchronization mechanisms.

Example: Sharing Data Between Threads

In Haskell, you can safely share immutable data across multiple threads without worrying about one thread changing the state and causing unexpected behavior in another thread.

Example using multiple threads:

import Control.Concurrent

-- Define an immutable value
sharedValue :: Int
sharedValue = 100

-- Define a function to simulate work on a thread
printSharedValue :: IO ()
printSharedValue = do
    putStrLn $ "Shared value is: " ++ show sharedValue

main = do
    -- Create two threads that both access the immutable shared value
    _ <- forkIO printSharedValue
    _ <- forkIO printSharedValue
    -- Wait for threads to finish
    threadDelay 1000000

In this example, two threads both access the immutable value sharedValue. Since the value is immutable, we can be sure that no matter how many threads access it, the value will remain unchanged and there will be no race conditions or synchronization issues.

Conclusion: Immutability is a Superpower

Immutability fundamentally changes the way we write programs by removing the complexities and risks associated with mutable state. By enforcing immutability, Haskell guarantees that:

	•	Your functions are pure and predictable.
	•	Your data is safe from unexpected changes.
	•	Your programs are inherently concurrent-safe, without the need for complex synchronization mechanisms.


## Recap & Exercises

### Recap

In this chapter, we’ve explored the concept of **immutability** in functional programming and its significant impact on how we write and reason about code. Let’s recap the key points:

- **Currying and Pure Evaluation**:
    - In Haskell, all functions are curried by default, meaning they can be partially applied. Immutability ensures that partially applied functions behave predictably.
    - Pure functions, which always return the same output for the same input and have no side effects, are central to immutability. They create predictable and easily testable code.

- **The Illusion of Mutability**:
    - In the real world, mutability doesn’t truly exist. Concepts like `x += 5` don’t make sense when modeling real-world behavior, as things change by creating new states, not modifying old ones.
    - The metaphor of the **bucket** illustrates how mutable state is confusing (anyone can throw things into the bucket), while immutable state keeps things clear and predictable (each person gets their own copy of the bucket).

- **Referential Transparency**:
    - Pure functions are referentially transparent, meaning any expression can be replaced by its value without changing the program’s behavior. This makes reasoning about and optimizing code easier.

- **Immutable Data Structures**:
    - Haskell’s lists, tuples, and records are all immutable. Once created, they cannot be changed. Instead of mutating data, we create new versions of the data.
    - While immutability might seem inefficient, Haskell optimizes performance with **persistent data structures** and **lazy evaluation**, ensuring that only necessary computations and memory allocations are performed.

- **Concurrency and Immutability**:
    - Immutability simplifies concurrency by removing the possibility of race conditions, deadlocks, or corrupted state. Immutable data can be safely shared across threads without any risk of interference.

By embracing immutability, Haskell makes it easier to write code that is predictable, scalable, and free from many of the pitfalls associated with mutable state. This drastically reduces bugs and complexity in concurrent and parallel systems.

### Exercises

The following exercises will help you practice using immutability in Haskell and reinforce the concepts discussed in this chapter.

---

#### Exercise 1: Implementing Pure Functions

**Objective**: Write a few pure functions that take inputs and return outputs without modifying any external state.

1. Write a pure function `square` that takes an integer and returns its square.

    ```haskell
    square :: Int -> Int
    -- Your implementation here
    ```

2. Write a pure function `calculateDiscount` that takes a price and a discount percentage and returns the price after applying the discount. The discount should be a `Float`.

    ```haskell
    calculateDiscount :: Float -> Float -> Float
    -- Your implementation here
    ```

3. Write a pure function `doubleList` that takes a list of integers and returns a new list with all integers doubled.

    ```haskell
    doubleList :: [Int] -> [Int]
    -- Your implementation here
    ```

**Hint**: Remember, these functions should not modify the input data. They should return new values based on the inputs.

---

#### Exercise 2: Working with Immutable Data Structures

**Objective**: Practice working with immutable data structures in Haskell by creating lists and modifying them without mutating the original data.

1. Define a list `numbers :: [Int]` that contains the integers 1 through 5.

    ```haskell
    numbers :: [Int]
    -- Your implementation here
    ```

2. Write a function `addElement` that takes an integer and a list of integers and returns a new list with the integer added to the front of the list.

    ```haskell
    addElement :: Int -> [Int] -> [Int]
    -- Your implementation here
    ```

3. Use the `addElement` function to add the number 6 to the `numbers` list, and then print both the original and the new list to verify that `numbers` has not been modified.

    ```haskell
    main :: IO ()
    main = do
        let newNumbers = addElement 6 numbers
        print numbers       -- Should still be [1, 2, 3, 4, 5]
        print newNumbers    -- Should be [6, 1, 2, 3, 4, 5]
    ```

**Hint**: Use the `:` operator to add an element to the front of the list.

---

#### Exercise 3: Managing State with Immutability

**Objective**: Practice managing state using immutable structures by implementing a simple counter.

1. Write a function `incrementCounter` that takes an integer (the current counter value) and returns the new counter value after incrementing it by 1.

    ```haskell
    incrementCounter :: Int -> Int
    -- Your implementation here
    ```

2. Write a function `updateCounter` that simulates a sequence of counter updates. It should take an initial counter value and a list of updates (e.g., `[+1, +1, -1]`), and return the final counter value.

    ```haskell
    updateCounter :: Int -> [Int] -> Int
    -- Your implementation here
    ```

3. Simulate counter updates and print the results. For example, starting with a counter of 5 and applying updates `[1, 1, -1]` should return 6.

    ```haskell
    main :: IO ()
    main = do
        let result = updateCounter 5 [1, 1, -1]
        print result  -- Should be 6
    ```

---

#### Exercise 4: Simulating State Changes with Pure Functions

**Objective**: Write a pure function that simulates state changes over time without using any mutable state.

1. Imagine a simple game where a character moves along a 2D grid. The state of the game is represented by the character’s coordinates (`(x, y)`).
   
   Write a function `moveCharacter` that takes the current position `(x, y)` and a direction (e.g., `Left`, `Right`, `Up`, `Down`), and returns the new position.

    ```haskell
    data Direction = Left | Right | Up | Down

    moveCharacter :: (Int, Int) -> Direction -> (Int, Int)
    -- Your implementation here
    ```

2. Simulate a sequence of moves using a list of directions and print the final position of the character. For example, starting at `(0, 0)` and moving `[Right, Up, Up, Left]` should return `(-1, 2)`.

    ```haskell
    main :: IO ()
    main = do
        let finalPosition = foldl moveCharacter (0, 0) [Right, Up, Up, Left]
        print finalPosition  -- Should print (-1, 2)
    ```

**Hint**: The `foldl` function can be used to apply `moveCharacter` to each direction in the list, updating the position step by step.
