-- HC8T1: Type Synonyms and Basic Function
type Address = String
type Value = Int

generateTx :: Address -> Address -> Value -> String
generateTx from to amount = "From: " ++ from ++ ", To: " ++ to ++ ", Amount: " ++ show amount

-- HC8T2: New Types and Data Constructors
data PaymentMethod = Cash | Card | Cryptocurrency deriving Show

data Person = PersonConstructor String (String, Int) PaymentMethod deriving Show

bob :: Person
bob = PersonConstructor "Bob" ("Main Street", 42) Cash

-- HC8T3: Algebraic Data Types and Functions
data Shape = Circle Float | Rectangle Float Float deriving Show

area :: Shape -> Float
area (Circle r) = pi * r * r
area (Rectangle w h) = w * h

-- HC8T4: Record Syntax for Employee
data Employee = Employee { name :: String, experienceInYears :: Float } deriving Show

richard :: Employee
richard = Employee { name = "Richard", experienceInYears = 7.5 }

-- HC8T5: Record Syntax for Person
data PersonRec = PersonRec { personName :: String, age :: Int, isEmployed :: Bool } deriving Show

person1 :: PersonRec
person1 = PersonRec "Alice" 30 True

person2 :: PersonRec
person2 = PersonRec "John" 45 False

-- HC8T6: Record Syntax for Shape Variants
data ShapeRec
    = CircleRec { center :: (Float, Float), color :: String, radius :: Float }
    | RectangleRec { width :: Float, height :: Float, color :: String }
    deriving Show

circleInstance :: ShapeRec
circleInstance = CircleRec { center = (0,0), color = "Red", radius = 10 }

rectangleInstance :: ShapeRec
rectangleInstance = RectangleRec { width = 20, height = 10, color = "Blue" }

-- HC8T7: Data Types and Describing Animals
data Animal = Dog String | Cat String deriving Show

describeAnimal :: Animal -> String
describeAnimal (Dog name) = "This is a dog named " ++ name
describeAnimal (Cat name) = "This is a cat named " ++ name

myDog :: Animal
myDog = Dog "Buster"

myCat :: Animal
myCat = Cat "Mittens"

-- HC8T8: Type Synonyms and Greeting Function
type Name = String
type Age = Int

greet :: Name -> Age -> String
greet name age = "Hello, " ++ name ++ "! You are " ++ show age ++ " years old."

-- HC8T9: Record Type and Transaction Function
data Transaction = Transaction { from :: Address, to :: Address, amount :: Value, transactionId :: String } deriving Show

createTransaction :: Address -> Address -> Value -> String
createTransaction fromAddr toAddr amt =
    let tx = Transaction fromAddr toAddr amt "TX123"
    in transactionId tx

-- HC8T10: Deriving Show for Book
data Book = Book { title :: String, author :: String, year :: Int } deriving Show

book1 :: Book
book1 = Book "Learn You a Haskell" "Miran Lipovača" 2011

-- MAIN
main :: IO ()
main = do
    putStrLn "== HC8T1: generateTx =="
    print (generateTx "addr1" "addr2" 50)

    putStrLn "\n== HC8T2: bob Person =="
    print bob

    putStrLn "\n== HC8T3: Shape areas =="
    print (area (Circle 5))
    print (area (Rectangle 10 5))

    putStrLn "\n== HC8T4: Employee richard =="
    print richard

    putStrLn "\n== HC8T5: Person1 (employed) and Person2 (unemployed) =="
    print person1
    print person2

    putStrLn "\n== HC8T6: Shape Record Instances =="
    print circleInstance
    print rectangleInstance

    putStrLn "\n== HC8T7: Animals and descriptions =="
    print myDog
    print (describeAnimal myDog)
    print myCat
    print (describeAnimal myCat)

    putStrLn "\n== HC8T8: Greeting function =="
    print (greet "Katlego" 22)

    putStrLn "\n== HC8T9: Create Transaction =="
    print (createTransaction "addrA" "addrB" 100)

    putStrLn "\n== HC8T10: Book Show instance =="
    print book1
