## Chapter 2: Immutability Changes Everything

### Introduction: Currying, Pure Evaluation, and the Power of Immutability

Immutability is at the core of functional programming, and it fundamentally changes the way we think about writing code and how programs execute. In Haskell, this isn’t just a recommendation—it’s enforced. All data in Haskell is immutable by default, meaning once a value is assigned, it cannot be changed.

In contrast, object-oriented programming (OOP) tends to favor **mutable state**—where data can be modified directly. This seems convenient at first but introduces many hidden dangers. Mutable state can cause unpredictable behavior and bugs because data can change in ways you don’t expect.

#### Currying and Pure Evaluation

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
