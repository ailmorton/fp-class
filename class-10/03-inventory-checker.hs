import Control.Monad
import Data.List
import System.Environment

{-
   Дан текстовый файл (inventory.txt)  с перечислением всей имеющейся на складе
   лёгкой брони. Сформируйте список имеющихся полных комплектов брони одного
   вида (kind). Указание: в решении рекомендуется пользоваться монадическими
   операциями всюду, где только возможно.
-}

data ArmorType = Shield | Helmet | Gauntlets | Boots | Cuirass
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorKind = Chitin | Hide | Leather | Elven | Scaled | Glass | ImperialLight
   deriving (Show, Read, Eq, Ord, Enum, Bounded)
data ArmorItem = ArmorItem ArmorKind ArmorType 
   deriving (Show, Eq)
data ArmorKit = ArmorKit ArmorKind [ArmorType]
   deriving (Show, Eq)

loadInventory :: FilePath -> IO [ArmorItem]
loadInventory fname = liftM (map makeArmor) (liftM lines (readFile fname))

makeArmor s = ArmorItem (kindFromString s1) (typeFromString s2)
	where [s1,s2] = words s

kindFromString s 
	| s=="Chitin" = Chitin
	| s=="Hide" = Hide
	| s=="Leather" = Leather
	| s=="Elven" = Elven
	| s=="Scaled" = Scaled
	| s=="Glass" = Glass
	| s=="ImperialLight" = ImperialLight

typeFromString s
	| s== "Shield" = Shield
	| s== "Helmet" = Helmet
	| s== "Gauntlets" = Gauntlets
	| s== "Boots" = Boots
	| s== "Cuirass" = Cuirass


buildArmorKit :: ArmorKind -> [ArmorItem] -> Maybe ArmorKit
buildArmorKit kind list = if (intersect [Shield, Helmet, Gauntlets, Boots, Cuirass] arm_types)==[Shield, Helmet, Gauntlets, Boots, Cuirass] then Just (ArmorKit kind arm_types) else Nothing
	where arm_types = (map (\(ArmorItem a b) -> b) (filter (\(ArmorItem a b) -> a==kind) list))

buildKits :: [ArmorItem] -> Maybe [ArmorKit]
buildKits list = if (arm_kits==Just []) then Nothing else arm_kits
	where arm_kits = foldr (\y z-> if y==Nothing then z else liftM2 (:) y z) (Just []) (map (\x -> buildArmorKit x list) [Chitin, Hide, Leather, Elven, Scaled, Glass, ImperialLight]) 


main = (head `liftM` getArgs) >>= loadInventory >>=  return . buildKits >>= print
