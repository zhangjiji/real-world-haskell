import qualified Data.Map as M

type PersonName = String
type PhoneNumber = String
type BillingAddress = String
data MobileCarrier = Honest_Bobs_Phone_Network
                   | Morrisas_Marvelous_Mobiles
                   | Peters_Plutocratic_Phones
                   deriving (Eq, Ord)

findCarrierBillingAddress :: PersonName
                             -> M.Map PersonName PhoneNumber
                             -> M.Map PhoneNumber MobileCarrier
                             -> M.Map MobileCarrier BillingAddress
                             -> Maybe BillingAddress

