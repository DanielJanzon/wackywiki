module Wiki (isCamel, extractLinks) where

alphabetUpper = ['A'..'Z'] ++ ['Å','Ä','Ö']
alphabetLower = ['a'..'z'] ++ ['å', 'ä', 'ö']

containsUpper :: String -> Bool
containsUpper = or . map (flip elem alphabetUpper)

containsLower :: String -> Bool
containsLower = or . map (flip elem alphabetLower)

isCamel str = head str `elem` alphabetUpper &&
              containsUpper (tail str) &&
              containsLower str

extractLinks :: String -> [String]
extractLinks = filter isCamel . words
