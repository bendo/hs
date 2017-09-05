import Data.List
import System.IO

foo = ("Foo Bar", 42)

foosName = fst foo

foosAge = snd foo

names = ["foo", "bar", "tom"]
addresses = ["123 Nord", "456 West", "789 East"]

namesAddresses = zip names addresses

idsNames = zip [1..] ["foo", "bar", "baz", "xyzzy"]
