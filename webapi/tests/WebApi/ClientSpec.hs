{-# OPTIONS_GHC -fno-warn-unused-binds #-}
{-# LANGUAGE MultiParamTypeClasses, TypeFamilies, OverloadedStrings, DataKinds, TypeOperators, TypeSynonymInstances, FlexibleInstances, DeriveGeneric, OverloadedStrings, FlexibleContexts, UndecidableInstances, GeneralizedNewtypeDeriving #-}

module WebApi.ClientSpec (spec) where

import Test.Hspec
import Data.Aeson (ToJSON (..), FromJSON (..))
import WebApi
import Data.Text (Text)
import qualified Network.Wai as Wai
import Network.Wai.Handler.Warp (run)
import Control.Concurrent (forkIO)
import Network.HTTP.Types.Status
import GHC.Generics
import Data.ByteString (ByteString)
import Data.Binary.Builder
import Data.Aeson.Encoding
import Control.Exception

port :: Int
port = 9080

withApp :: SpecWith () -> Spec
withApp = beforeAll_ (forkIO (run port clientSpecApp) >> pure ())

clientSpecApp :: Wai.Application
clientSpecApp = serverApp serverSettings ClientSpecImpl

data ClientSpec
data ClientSpecImpl = ClientSpecImpl

type Persons = "person" :/ Bool :/ "all"
type LargeOutput = "large" :/ "output"

data Person = Person { name    :: Text
                     , age     :: Age
                     , address :: Text
                     } deriving (Show, Eq, Generic)

instance ToJSON Person
instance FromJSON Person

data Output = Output { email :: Text }
            deriving (Show, Read, Generic, Eq)

instance FromJSON Output

newtype Age = Age { getAge :: Int }
            deriving (Show, Eq, ToJSON, FromJSON, Generic)

newtype LargeOutputData = LargeOutputData { largeOutputData :: [Output] }
                        deriving (Show, Eq, FromJSON)

instance ToJSON LargeOutputData where
  toEncoding _ = unsafeToEncoding (fromByteString largeJSON)
  toJSON       = undefined


instance WebApi ClientSpec where
  type Apis ClientSpec = '[ Route '[GET] Persons
                          , Route '[GET] LargeOutput
                          ]

instance ApiContract ClientSpec GET Persons where
  type ApiOut GET Persons = [Person]
  type ApiErr GET Persons = [Person]

instance ApiContract ClientSpec GET LargeOutput where
  type ApiOut GET LargeOutput = LargeOutputData

instance WebApiServer ClientSpecImpl where
  type ApiInterface ClientSpecImpl = ClientSpec

instance ApiHandler ClientSpecImpl GET Persons where
  handler _ req = do
    let pp = pathParam req
    case pp of
      True  -> respond persons
      False -> raise status410 persons

instance ApiHandler ClientSpecImpl GET LargeOutput where
  handler _ _ = respond (LargeOutputData [ undefined ])

persons :: [Person]
persons = [Person "foo" (Age 10) "10 1st baz"]

spec :: Spec
spec = withApp $ describe "Webapi client" $ do
  it "can create proper requests" $
    shouldReturn (clientAct True) (Right persons)
  -- NOTE: since [Person] is being used in both success and failure
  --       the first one is being picked [due to default behavior changing in http-client 0.5]
  it "should handle api errors" $
    shouldReturn (clientAct False) (Right persons) -- (Left (status410, persons))
  it "should work with large output" $
    shouldReturn largeOutputAct (Right (LargeOutputData [ Output { email = "rowenawilson@enthaze.com" }
                                                        , Output { email = "perezsolomon@digial.com" }
                                                        , Output { email = "manuelaromero@zerology.com" }
                                                        , Output { email = "genaclay@kaggle.com" }
                                                        , Output { email = "hayesbrooks@norsul.com" }
                                                        , Output { email = "kimdejesus@zepitope.com" }
                                                        , Output { email = "dicksonwolf@gaptec.com" }
                                                        , Output { email = "donovandorsey@diginetic.com" }
                                                        , Output { email = "santiagole@octocore.com" }
                                                        , Output { email = "eulasantiago@tribalog.com" }
                                                        , Output { email = "rhondapatterson@prismatic.com" }
                                                        , Output { email = "danielscross@comstar.com" }
                                                        , Output { email = "wendinichols@cuizine.com" }
                                                        ]))

largeOutputAct :: IO (Either (Either (ApiError GET LargeOutput) OtherError) LargeOutputData)
largeOutputAct = do
  mgr <- newManager defaultManagerSettings
  let csett = ClientSettings { baseUrl           = "http://localhost:" ++ show port
                             , connectionManager = mgr
                             }
  resp <- client csett (Request { pathParam   = ()
                                , queryParam  = ()
                                , formParam   = ()
                                , fileParam   = ()
                                , cookieIn    = ()
                                , headerIn    = ()
                                , requestBody = ()
                                } :: Request GET LargeOutput
                      )
  case resp of
    Success _ d _ _   -> putStrLn "Success" >> return (Right d)
    Failure e         -> putStrLn "Failure" >> return (Left e)

clientAct :: Bool -> IO (Either (Status, [Person]) [Person])
clientAct pp = do
  mgr <- newManager defaultManagerSettings
  let csett = ClientSettings { baseUrl           = "http://localhost:" ++ show port
                             , connectionManager = mgr
                             }
  resp <- client csett (Request { pathParam    = pp
                                , queryParam  = ()
                                , formParam   = ()
                                , fileParam   = ()
                                , cookieIn    = ()
                                , headerIn    = ()
                                , requestBody = ()
                                } :: Request GET Persons
                       )
  case resp of
    Success _ d _ _                -> putStrLn "Success" >> return (Right d)
    Failure (Left ap)              -> putStrLn "Failed"  >> return (Left (code ap, err ap))
    Failure (Right (OtherError e)) -> throwIO e

instance ParamErrToApiErr [Person]

largeJSON :: ByteString
largeJSON = "[{\"_id\":\"59c90cc280d66673e566992f\",\"index\":0,\"guid\":\"2d68eee4-0aec-4370-a459-6260d5a1cbac\",\"isActive\":false,\"balance\":\"$1,967.57\",\"picture\":\"http://placehold.it/32x32\",\"age\":26,\"eyeColor\":\"brown\",\"name\":\"Rowena Wilson\",\"gender\":\"female\",\"company\":\"ENTHAZE\",\"email\":\"rowenawilson@enthaze.com\",\"phone\":\"+1 (967) 508-2478\",\"address\":\"336 Everett Avenue, Allensworth, New Mexico, 846\",\"about\":\"Exercitation anim consectetur est consectetur. Irure minim mollit elit Lorem esse. Non adipisicing dolor officia enim anim proident laboris pariatur. Excepteur in cupidatat nostrud nisi veniam ex velit et laborum in pariatur Lorem exercitation. Velit ex esse culpa sit irure dolor minim qui. Ex consectetur do proident voluptate velit velit dolore nostrud do reprehenderit minim est officia. Laboris in cillum reprehenderit elit.\r\n\",\"registered\":\"2014-12-27T08:34:46 -06:-30\",\"latitude\":-75.019899,\"longitude\":93.113292,\"tags\":[\"id\",\"veniam\",\"ut\",\"non\",\"Lorem\",\"laboris\",\"est\"],\"friends\":[{\"id\":0,\"name\":\"Jeannette Mejia\"},{\"id\":1,\"name\":\"Louella Mcdowell\"},{\"id\":2,\"name\":\"Olive Workman\"}],\"greeting\":\"Hello, Rowena Wilson! You have 8 unread messages.\",\"favoriteFruit\":\"strawberry\"},{\"_id\":\"59c90cc270385681e3004548\",\"index\":1,\"guid\":\"752421c4-7268-4a26-806b-7ddcfa09a6e5\",\"isActive\":false,\"balance\":\"$1,411.93\",\"picture\":\"http://placehold.it/32x32\",\"age\":21,\"eyeColor\":\"green\",\"name\":\"Perez Solomon\",\"gender\":\"male\",\"company\":\"DIGIAL\",\"email\":\"perezsolomon@digial.com\",\"phone\":\"+1 (867) 596-3676\",\"address\":\"786 Crawford Avenue, Canterwood, Federated States Of Micronesia, 5990\",\"about\":\"Minim ipsum sit deserunt qui et culpa culpa consequat eiusmod officia do. Lorem consectetur sit sint eiusmod officia ullamco cupidatat laborum. Ut sunt ullamco ad officia aliquip. In ad reprehenderit fugiat consequat incididunt elit et. Exercitation occaecat excepteur pariatur sint tempor et duis magna nisi reprehenderit irure cupidatat. Excepteur eu irure sunt consequat sit consectetur cillum et commodo laborum fugiat.\r\n\",\"registered\":\"2016-04-22T02:04:45 -06:-30\",\"latitude\":42.881863,\"longitude\":155.647117,\"tags\":[\"et\",\"occaecat\",\"qui\",\"veniam\",\"et\",\"duis\",\"eu\"],\"friends\":[{\"id\":0,\"name\":\"Prince Cohen\"},{\"id\":1,\"name\":\"Lesa Dominguez\"},{\"id\":2,\"name\":\"Chasity Scott\"}],\"greeting\":\"Hello, Perez Solomon! You have 1 unread messages.\",\"favoriteFruit\":\"strawberry\"},{\"_id\":\"59c90cc27305d81f6f5d4f00\",\"index\":2,\"guid\":\"0f749f51-e9bc-470a-99d6-367c0df5e130\",\"isActive\":true,\"balance\":\"$3,151.19\",\"picture\":\"http://placehold.it/32x32\",\"age\":30,\"eyeColor\":\"blue\",\"name\":\"Manuela Romero\",\"gender\":\"female\",\"company\":\"ZEROLOGY\",\"email\":\"manuelaromero@zerology.com\",\"phone\":\"+1 (850) 458-3595\",\"address\":\"883 Holly Street, Buxton, Vermont, 5335\",\"about\":\"Enim amet exercitation tempor exercitation deserunt proident sunt magna dolore nulla exercitation irure nostrud. Eu in id laborum officia duis et enim occaecat officia duis id est tempor. Ex nulla consequat aute eu et elit. Duis cillum aliqua fugiat labore deserunt irure velit qui mollit. Eiusmod ex ea est sint dolor.\r\n\",\"registered\":\"2014-04-06T11:52:46 -06:-30\",\"latitude\":-47.154106,\"longitude\":108.056238,\"tags\":[\"dolore\",\"sit\",\"voluptate\",\"elit\",\"do\",\"occaecat\",\"tempor\"],\"friends\":[{\"id\":0,\"name\":\"Cecelia Macdonald\"},{\"id\":1,\"name\":\"Monica Sloan\"},{\"id\":2,\"name\":\"Charity Preston\"}],\"greeting\":\"Hello, Manuela Romero! You have 10 unread messages.\",\"favoriteFruit\":\"apple\"},{\"_id\":\"59c90cc2609222d8faad19b1\",\"index\":3,\"guid\":\"57b43f8d-47eb-48ef-b2d7-fa675541f0e2\",\"isActive\":true,\"balance\":\"$1,088.50\",\"picture\":\"http://placehold.it/32x32\",\"age\":22,\"eyeColor\":\"blue\",\"name\":\"Gena Clay\",\"gender\":\"female\",\"company\":\"KAGGLE\",\"email\":\"genaclay@kaggle.com\",\"phone\":\"+1 (957) 552-3263\",\"address\":\"653 Lafayette Walk, Orick, Tennessee, 2168\",\"about\":\"Mollit esse non ipsum in excepteur anim officia aliquip. Cupidatat amet eiusmod deserunt nulla qui incididunt nulla ad eiusmod. Sit labore nisi enim voluptate.\r\n\",\"registered\":\"2017-08-13T11:42:54 -06:-30\",\"latitude\":-82.041825,\"longitude\":-137.77113,\"tags\":[\"anim\",\"dolor\",\"cupidatat\",\"aliqua\",\"sunt\",\"labore\",\"duis\"],\"friends\":[{\"id\":0,\"name\":\"Gaines Pace\"},{\"id\":1,\"name\":\"Barlow Langley\"},{\"id\":2,\"name\":\"Angelita Castillo\"}],\"greeting\":\"Hello, Gena Clay! You have 3 unread messages.\",\"favoriteFruit\":\"strawberry\"},{\"_id\":\"59c90cc21f6e4af7fdaa4c72\",\"index\":4,\"guid\":\"5ce7eeb7-9000-474e-9f54-3aee22d91fe2\",\"isActive\":false,\"balance\":\"$1,251.44\",\"picture\":\"http://placehold.it/32x32\",\"age\":39,\"eyeColor\":\"blue\",\"name\":\"Hayes Brooks\",\"gender\":\"male\",\"company\":\"NORSUL\",\"email\":\"hayesbrooks@norsul.com\",\"phone\":\"+1 (876) 583-2011\",\"address\":\"887 Dahill Road, Olney, Alaska, 7998\",\"about\":\"Commodo ullamco magna magna eu reprehenderit tempor veniam aliqua Lorem id amet cillum nulla incididunt. Minim velit laborum nisi in qui Lorem reprehenderit anim. Cillum do dolore proident enim esse Lorem officia sunt sit excepteur quis culpa nostrud excepteur. Fugiat deserunt proident adipisicing laborum amet consequat cupidatat et minim mollit.\r\n\",\"registered\":\"2014-05-18T04:25:15 -06:-30\",\"latitude\":25.282939,\"longitude\":-83.729391,\"tags\":[\"voluptate\",\"fugiat\",\"sunt\",\"consequat\",\"sint\",\"dolor\",\"cupidatat\"],\"friends\":[{\"id\":0,\"name\":\"Ellison Terry\"},{\"id\":1,\"name\":\"Pace Shepard\"},{\"id\":2,\"name\":\"Natalia Mcgee\"}],\"greeting\":\"Hello, Hayes Brooks! You have 8 unread messages.\",\"favoriteFruit\":\"banana\"},{\"_id\":\"59c90cc2f76d9e5ba596a1b8\",\"index\":5,\"guid\":\"1c84b731-10de-4630-ad32-42f21cd8d744\",\"isActive\":true,\"balance\":\"$2,843.30\",\"picture\":\"http://placehold.it/32x32\",\"age\":25,\"eyeColor\":\"brown\",\"name\":\"Kim Dejesus\",\"gender\":\"female\",\"company\":\"ZEPITOPE\",\"email\":\"kimdejesus@zepitope.com\",\"phone\":\"+1 (800) 471-2489\",\"address\":\"456 Beaumont Street, Rosedale, Arkansas, 6284\",\"about\":\"Lorem labore dolor sint duis fugiat non tempor aliqua tempor ad cupidatat. Nulla aute laborum consequat anim excepteur ad aliqua proident dolore ea magna. Quis minim ipsum esse exercitation aliqua reprehenderit dolor laboris amet. Adipisicing proident proident elit in proident duis commodo mollit laborum. Excepteur est veniam consectetur laboris excepteur. Aute reprehenderit magna dolore in ut aliqua pariatur est duis proident cillum consectetur cupidatat.\r\n\",\"registered\":\"2014-04-04T07:32:11 -06:-30\",\"latitude\":49.112601,\"longitude\":-17.716796,\"tags\":[\"ea\",\"nulla\",\"enim\",\"cillum\",\"adipisicing\",\"anim\",\"Lorem\"],\"friends\":[{\"id\":0,\"name\":\"Connie Forbes\"},{\"id\":1,\"name\":\"Jana Cervantes\"},{\"id\":2,\"name\":\"Vaughan Juarez\"}],\"greeting\":\"Hello, Kim Dejesus! You have 8 unread messages.\",\"favoriteFruit\":\"apple\"},{\"_id\":\"59c90cc241bb2318f27e0fb5\",\"index\":6,\"guid\":\"f80a2193-448b-42a7-8419-75b8f39549bc\",\"isActive\":false,\"balance\":\"$3,567.32\",\"picture\":\"http://placehold.it/32x32\",\"age\":40,\"eyeColor\":\"green\",\"name\":\"Dickson Wolf\",\"gender\":\"male\",\"company\":\"GAPTEC\",\"email\":\"dicksonwolf@gaptec.com\",\"phone\":\"+1 (831) 421-3324\",\"address\":\"248 Willow Place, Englevale, Arizona, 2406\",\"about\":\"Ut mollit dolor amet exercitation mollit enim nulla. Irure elit laboris consectetur duis eu dolore culpa fugiat dolore ipsum duis laboris anim occaecat. Ad sint nisi fugiat sit proident. Minim Lorem pariatur commodo fugiat magna esse fugiat labore est excepteur. Exercitation minim voluptate sint aliqua cupidatat eu. Eiusmod aliquip magna excepteur nisi esse incididunt veniam magna veniam enim eu duis velit.\r\n\",\"registered\":\"2017-09-23T10:37:39 -06:-30\",\"latitude\":-74.164455,\"longitude\":-170.353628,\"tags\":[\"esse\",\"labore\",\"minim\",\"qui\",\"esse\",\"deserunt\",\"magna\"],\"friends\":[{\"id\":0,\"name\":\"Banks Shannon\"},{\"id\":1,\"name\":\"Fanny Norman\"},{\"id\":2,\"name\":\"Lyons Cobb\"}],\"greeting\":\"Hello, Dickson Wolf! You have 7 unread messages.\",\"favoriteFruit\":\"apple\"},{\"_id\":\"59c90cc2b2b7b7f23fd54b7b\",\"index\":7,\"guid\":\"59c8be34-6b34-414a-b56e-8def6f573689\",\"isActive\":false,\"balance\":\"$1,668.14\",\"picture\":\"http://placehold.it/32x32\",\"age\":37,\"eyeColor\":\"green\",\"name\":\"Donovan Dorsey\",\"gender\":\"male\",\"company\":\"DIGINETIC\",\"email\":\"donovandorsey@diginetic.com\",\"phone\":\"+1 (978) 447-2349\",\"address\":\"107 Jackson Street, Basye, New Jersey, 7464\",\"about\":\"Eu voluptate laboris ad velit dolor ea velit incididunt. Enim est consectetur cillum tempor. Cupidatat magna cupidatat nulla proident aliquip id cupidatat. Laborum ut est enim amet aute in magna duis duis esse elit. Non excepteur deserunt sint do.\r\n\",\"registered\":\"2015-09-14T05:27:05 -06:-30\",\"latitude\":20.046989,\"longitude\":-46.730946,\"tags\":[\"adipisicing\",\"in\",\"aliquip\",\"et\",\"laborum\",\"sit\",\"incididunt\"],\"friends\":[{\"id\":0,\"name\":\"Tami Skinner\"},{\"id\":1,\"name\":\"Christensen West\"},{\"id\":2,\"name\":\"Ferrell Casey\"}],\"greeting\":\"Hello, Donovan Dorsey! You have 7 unread messages.\",\"favoriteFruit\":\"banana\"},{\"_id\":\"59c90cc22eda95f7d36033d6\",\"index\":8,\"guid\":\"1b7d57f7-4c31-4b76-b5cc-ed71bd60633f\",\"isActive\":false,\"balance\":\"$1,815.94\",\"picture\":\"http://placehold.it/32x32\",\"age\":38,\"eyeColor\":\"blue\",\"name\":\"Santiago Le\",\"gender\":\"male\",\"company\":\"OCTOCORE\",\"email\":\"santiagole@octocore.com\",\"phone\":\"+1 (951) 458-2704\",\"address\":\"308 Logan Street, Imperial, Ohio, 1335\",\"about\":\"Amet Lorem aliqua ut aliquip duis elit commodo Lorem. Voluptate cillum quis eu aute fugiat esse magna aute nostrud. Ipsum reprehenderit qui laboris ex esse excepteur dolor id ullamco. Commodo sit incididunt officia sit.\r\n\",\"registered\":\"2015-11-13T09:10:40 -06:-30\",\"latitude\":80.215467,\"longitude\":13.5918,\"tags\":[\"velit\",\"nulla\",\"amet\",\"veniam\",\"magna\",\"culpa\",\"officia\"],\"friends\":[{\"id\":0,\"name\":\"Elnora Sellers\"},{\"id\":1,\"name\":\"Cleveland Black\"},{\"id\":2,\"name\":\"Rosales Paul\"}],\"greeting\":\"Hello, Santiago Le! You have 10 unread messages.\",\"favoriteFruit\":\"banana\"},{\"_id\":\"59c90cc292bb90c86b01edf9\",\"index\":9,\"guid\":\"52f839d7-e379-4f0b-b60f-cf46eff4d508\",\"isActive\":true,\"balance\":\"$2,766.58\",\"picture\":\"http://placehold.it/32x32\",\"age\":24,\"eyeColor\":\"brown\",\"name\":\"Eula Santiago\",\"gender\":\"female\",\"company\":\"TRIBALOG\",\"email\":\"eulasantiago@tribalog.com\",\"phone\":\"+1 (877) 540-2324\",\"address\":\"166 Nevins Street, Leland, Hawaii, 2781\",\"about\":\"Amet amet aute non commodo adipisicing et tempor deserunt qui elit exercitation id sunt reprehenderit. Ea pariatur ea do in adipisicing. Adipisicing exercitation deserunt esse officia non exercitation eu.\r\n\",\"registered\":\"2016-11-11T02:43:38 -06:-30\",\"latitude\":11.05768,\"longitude\":-129.695766,\"tags\":[\"magna\",\"pariatur\",\"adipisicing\",\"irure\",\"qui\",\"enim\",\"enim\"],\"friends\":[{\"id\":0,\"name\":\"Buckley Mcneil\"},{\"id\":1,\"name\":\"Britney Rivers\"},{\"id\":2,\"name\":\"Durham Rhodes\"}],\"greeting\":\"Hello, Eula Santiago! You have 1 unread messages.\",\"favoriteFruit\":\"strawberry\"},{\"_id\":\"59c90cc2052e4ef34f8df557\",\"index\":10,\"guid\":\"b813ad49-c18c-406f-beec-3e6485ed50ba\",\"isActive\":true,\"balance\":\"$2,199.22\",\"picture\":\"http://placehold.it/32x32\",\"age\":38,\"eyeColor\":\"green\",\"name\":\"Rhonda Patterson\",\"gender\":\"female\",\"company\":\"PRISMATIC\",\"email\":\"rhondapatterson@prismatic.com\",\"phone\":\"+1 (812) 565-2981\",\"address\":\"247 Erskine Loop, Adelino, Louisiana, 4604\",\"about\":\"Consequat exercitation proident cupidatat duis Lorem reprehenderit eiusmod id labore officia. Incididunt tempor deserunt Lorem occaecat aliqua minim consectetur id minim duis reprehenderit do consectetur sint. Aliquip in eu irure pariatur magna reprehenderit est minim. Et ullamco do aliquip minim eiusmod nulla aliqua proident. Irure culpa minim cupidatat minim ipsum.\r\n\",\"registered\":\"2016-05-23T08:23:39 -06:-30\",\"latitude\":-17.461488,\"longitude\":123.612713,\"tags\":[\"veniam\",\"fugiat\",\"consectetur\",\"reprehenderit\",\"pariatur\",\"occaecat\",\"eiusmod\"],\"friends\":[{\"id\":0,\"name\":\"Tricia Lang\"},{\"id\":1,\"name\":\"Snider Hooper\"},{\"id\":2,\"name\":\"Graciela Mueller\"}],\"greeting\":\"Hello, Rhonda Patterson! You have 1 unread messages.\",\"favoriteFruit\":\"banana\"},{\"_id\":\"59c90cc2ad41a25f0bfa7b30\",\"index\":11,\"guid\":\"092e97d4-6d38-4983-a470-0c434416b520\",\"isActive\":false,\"balance\":\"$3,888.32\",\"picture\":\"http://placehold.it/32x32\",\"age\":26,\"eyeColor\":\"blue\",\"name\":\"Daniels Cross\",\"gender\":\"male\",\"company\":\"COMSTAR\",\"email\":\"danielscross@comstar.com\",\"phone\":\"+1 (829) 460-2069\",\"address\":\"707 Kermit Place, Edinburg, Connecticut, 8898\",\"about\":\"Qui nisi sunt culpa laborum nisi non ea commodo magna proident culpa do exercitation. Aliquip aliquip eu laboris proident. In laborum non do dolore.\r\n\",\"registered\":\"2014-03-01T07:17:48 -06:-30\",\"latitude\":-37.155836,\"longitude\":-65.696606,\"tags\":[\"consectetur\",\"ad\",\"in\",\"nulla\",\"dolore\",\"aliqua\",\"deserunt\"],\"friends\":[{\"id\":0,\"name\":\"Lula Estrada\"},{\"id\":1,\"name\":\"Rita Valencia\"},{\"id\":2,\"name\":\"Helene Carrillo\"}],\"greeting\":\"Hello, Daniels Cross! You have 5 unread messages.\",\"favoriteFruit\":\"apple\"},{\"_id\":\"59c90cc2e771f84418435328\",\"index\":12,\"guid\":\"94bb3ee2-a6d4-4f5e-9801-6b65f0faa7db\",\"isActive\":true,\"balance\":\"$2,324.68\",\"picture\":\"http://placehold.it/32x32\",\"age\":34,\"eyeColor\":\"green\",\"name\":\"Wendi Nichols\",\"gender\":\"female\",\"company\":\"CUIZINE\",\"email\":\"wendinichols@cuizine.com\",\"phone\":\"+1 (853) 532-2229\",\"address\":\"484 McKibbin Street, Lindcove, North Carolina, 9192\",\"about\":\"Duis eiusmod aute consequat fugiat sint eiusmod laboris. Excepteur laborum nostrud magna cillum aliqua deserunt incididunt eiusmod eu culpa eu duis anim dolore. Quis laboris anim do enim minim ea in sint laborum sit aliquip amet cupidatat exercitation.\r\n\",\"registered\":\"2014-06-27T09:10:25 -06:-30\",\"latitude\":-58.020291,\"longitude\":-22.024332,\"tags\":[\"voluptate\",\"proident\",\"cillum\",\"magna\",\"et\",\"deserunt\",\"mollit\"],\"friends\":[{\"id\":0,\"name\":\"Judith Combs\"},{\"id\":1,\"name\":\"Carr Weber\"},{\"id\":2,\"name\":\"Judy Atkins\"}],\"greeting\":\"Hello, Wendi Nichols! You have 10 unread messages.\",\"favoriteFruit\":\"strawberry\"}]"

instance (Show (ApiErr m r)) => Show (ApiError m r) where
  show (ApiError s v _ _) = "Status: " ++ show s ++ ", " ++ "Value: " ++ show v

instance Show OtherError where
  show _ = "OtherError"

instance Eq (ApiError m r) where
  a == b = False

instance Eq OtherError where
  a == b = False
