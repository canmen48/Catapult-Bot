# Catapult Bot (Haskell)

This project implements a bot for the abstract strategy game **Catapult** in Haskell.  
The assignment is part of the *Softwaretechnik und Programmierparadigmen* course (WiSe 2024/2025, TU Berlin).  

## Project Structure
src/Board.hs -- Board-related functions
src/Catapult.hs -- Catapult logic and move generation
test/Spec.hs -- Unit tests (HSpec)

## Project Structure
# Test Commands:
- Run all tests: stack test
- Run validation tests: stack test catapult:validate
- Run unit tests: stack test catapult:units
- Run grading tests: stack test catapult:grading
- Run tests with coverage: stack test --coverage ...

# Building Commands
- stack build
- stack clean
