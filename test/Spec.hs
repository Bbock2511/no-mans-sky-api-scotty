import Test.HUnit
import System.Exit
import Model.GalaxySpec (galaxyTests)
import Model.SolarSystemSpec (solarSystemTests)
import Model.PlanetSpec (planetTests)
import Model.MsgSpec (msgTests)

main :: IO ()
main = do
    -- Agrupa todas as suítes de teste
    let allTests = TestList
            [ galaxyTests
            , solarSystemTests
            , planetTests
            , msgTests
            ]

    -- Roda os testes
    counts <- runTestTT allTests
    
    -- Finaliza com um código de erro se algum teste falhar
    if errors counts > 0 || failures counts > 0
        then exitFailure
        else exitSuccess