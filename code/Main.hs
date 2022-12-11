import Enigma

enigma1 = (SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25))
plugboard = [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')] 
enigma2 = (SteckeredEnigma rotor1 rotor2 rotor3 reflectorB (0,0,25) [('F','T'),('D','U'),('V','A'),('K','W'),('H','Z'),('I','X')])
enigma3 = (SteckeredEnigma rotor2 rotor4 rotor3 reflectorB (0,0,25) [('F','X'),('D','M'),('E','A'),('S','T')])
enigma4 = (SteckeredEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25) [('C','R'),('L','N'),('E','P'),('S','I')])

-- this should gen longest menu of length 7
-- []
testEnigma1 = SimpleEnigma rotor3 rotor2 rotor1 reflectorB (0,0,25)
testEnigma = SimpleEnigma rotor1 rotor2 rotor3 reflectorB (3,9,2)
p1 = "WETTERVORHERSAGEBISKAYA"
x1 = encodeMessage p1 testEnigma

z1 = "AFJEQTMC"
z2 =  "FJEQTMCF"

p0 = "APPLE"
x0 = encodeMessage p0 testEnigma

simpleCrib :: [(Int,Char, Char)]
simpleCrib = [(0,'M','K'),(1,'N','G'),(2,'G','F'),(3,'G','B'),(4,'B','L')]

-- simple text cribs
cribxy = [('Q','R'),('M','L'),('F','K'),('B','J'),('K','M'),('F','B'), ('F','D'), ('J','Z'),('T','F'),('L','D')]

cribb = [('W','R'),('E','W'),('T','I'),('T','V'),('E','T'),('R','Y'),('V','R'),('O','E'),('R','S'),('H','X'),('E','B'),('R','F'),('S','O'),('A','G'),('G','K'),('E','U'),('B','H'),('I','Q'),('S','B'),('K','A'),('A','I'),('Y','S'),('A','E')]

p2 =  "THEMAINPROFESSIONALINSTITUTIONFORCOMPUTING"
x2a = "QKPKFNLRTLQHVIGRIKUOEWSXRTIHWIODZORBRQJFZLMAJXKVXBPHROGJSPVIFNQERCRSZATNJXORIKPYEDBMYCLOCTHAZCSFSTGMRJCVICAJDUUETLYLFGJAFQZTDUTVPPYVROVPCGFYZTKWJEAKZZQXQCQUBMKDHYHAVIXKDLHRFCGTAADGJZLTVTWAKXHZNDFVRBUBTLWGBYXSOTVGMPSKCODKAOUCQJPJPEOIMAEMSSNVINMVVLFCMGVQCQOKAEWLVBSRBKTMXRUJOSUTCWRDHDYPHPHNVLEGVHERDSMTEUUQNRNVALCZGKRKXTNDCLLTQHVBOQYMYQMCZEZYBDKQOXRVFZBEUUYMKIYZNBDMVSFXWPUMDMGTCABLEBAPAKFQCXOPWIVDTMTIKCVIRALQKVSLVQHBFOMWRVKOWWJRMCBBQETMFBQBDCIMXMZPFJYNPVDJJFJMYGKHFDLQCYTFBXGQGRVQFKUHJQYTXQPSAJUHLSVKMJOSKRUHLDADOKQQPVEJGPWLNHYJUBDHCVBMMEPAJLUSECKYLCFMVFQLTPBYOHREQMOCDTWUXOGREDIVWJSXKHWJTKUHZHVEIIKGEEVZIMLKLMJAVYCIRWBQCSVXLTJBEXZYDJNTLWTRPNCMJWSPWQUGBUDLWQRUFYMHMNAASAHRMDREKWHTIOVZMBOTDUZHMWSMJNJIJWHEBBVGJKTHHYYCWUMTGJKROEZZKJZQDJUIOUPVIIYZIROQISYALZDHUPYTHYTLOPLFDKQZMBUOCXNOBUGGGCGQMXKUJKUUPFFGAJJIKCZRENJQLHCKTMUDFQKDZYCVGIULRKGUCOBJIGSESZXSCJYWHXGBDYGCHCIFWQYPVMBBHYNLKSZRFVYIFFNYEETKHLNLBGKWBGRIMFOVEIWHRIYQSVADSDKPYDOIKXQTUYRHPXJZFHYUQADBVLVDGMDCGLOORPZQBTVNBAPIJGSMJRHKFKSRSFSUVZDGYCSQFXDMKWKNACWXQOSQUOCJGICHNGRKIAXKQMYWHDFDEEKZJZQDBQCTAXSZYUHXLHMYJ"
x2b = "QKPKFNLRTLQHVEGRIKUOEWSXRTIHWIODZORBRQJSZLMAJXKVXBPHROGJSPVIFNQERGDDANKEGLZLUVCRHOCESEHBSQXQDZZKVMIAABJERUTULCVSZQKEZLTOMSNLIHXKVFTYPLDUNSCPCGRJGMLVOPEMKFKOWWNUAGWGEGOELYJENXUSQICSJULMANOWVWMRSXKUVCKOOILRDQTPLUMTQBMSGYUMZOMILBJRTXFMYQCVXFBTUKHCSVGSYOUICPPEOCHTPVFDMKMSQHZBEUZVIXLIMASEFVINKNDMXFKRVOPGOGWFFTLGSQPOAPENFIWOCPKTDJIBYXBZOACKOSAWUNFVEGDPAMZQMAAAZAKTHTCBOVBPUUJAYDSGJRETBUBYXLKIOSKROZLKUFIMMAMFBUZKFLGCQKJXAYJZPWUZXBGIWZNPYPELVJUSBMGFMKPMJXWOCFWQDTZTRSOBMRKKQGGQZRHVMUACCBQKBVAHOELDNTXXWFHIQOGPOFAGZUJMROBDMSMPUGMNZVSZXXVJFCNWVAMOWPVGPJGMPZHEXKHRIXIFCJMXZCPTTOYJOVNYSVWCXBBKCNVRAGJQWXYIXDAOEBQUZMTUSOBEGLKCLZEULAIBDACUXLXVYRWVQIGWDRYRSQIUUFDJUBPGGDVZHDBYQBMNWSSWLFGXMGANDUWKAWLEIKMOLNNAXKDPHNULCGFTSYYZNCOKZJRPDRYXHIQWAPLVBGQULAZPUIJSCISWPGMMUKEMYFWCKCWXCJOTRZVPFLYADFZWNMBHLZQPUAHYFGGHCCKJSSBTMPUBFWLEWNWJSDYFCKFAVUHDYYGNYADBPTFQMXJZPQTHVSYSLHSTOBWGKDYEPRFNLZJEGOKJMXDZBXRLGTGHOAIDZVTJZSPQAEHKHRBFDLOCDQOAJEYACHXMXFMFYUODNVPIJZNYVIFHZWHRJVQKGTQANDUYZTGRRFQVQMLNJDJAXSTLVIMEPYPHEZMYYKHXGSCZBCRDHXHMEGFCOTHCDVETICYWEDVPXJGJLAAOQLANLGPEUUSIKQMWXNTYHOPEG"
x2c = "QKPKFNLRTLQHVIGRIKUOEWSXRTIHWIODZORBRQJFZLMAJXKVXBPHROGJSPVIFNQERCDDANKEGLZLUVCRHOCESEHBSQXGPBLUZDVKIWNKNPQTBBSRQVBTGFTOMSNLIHXKVFTYPLDUNSCPCGRDGMLVOPEMKFKOWWNUAGWGEGOELKJENXUSQICSJULMANOWVWMRSXKPVCKOOILRDQTPLUMTQBMSGYUMZGMILBJRTXFMYQCVXFBTUKHCSVGIYOUICPPEOCHTPVFDMKMSQHZBEYZVIXLIMASEFVINKNDMXFKRVOPVOGWFFTLGSQPOAPENFIWOCPKTDDIBYXBZOACKOSAWUNFVEGDPAMZWMAAAZAKTHTCBOVBPUUJAYDSGJMETBUBYXLKIOSKROZLKUFIMMAMKBUZKFLGCQKJXAYJZPWUZXBGIWQNPYPELVJUSBMGFMKPMJXWOCFWCDTZTRSOBMRKKQGGQZRHVMUACCIQKBVAHOELDNTXXWFHIQOGPOFAUZUJMROBDMSMPUGMNZVSZXXVJFZNWVAMOWPVGPJGMPZHEXKHRIXINCJMXZCPTTOYJOVNYSVWCXBBKCYVRAGJQWXYIXDAOEBQUZMTUSOBEGLKCLZEULAIBDACUXLXVYRWVQMGWDRYRSQIUUFDJUBPGGDVZHDBKQBMNWSSWLFGXMGANDUWKAWLEIHMOLNNAXKDPHNULCGFTSYYZNCOAZJRPDRYXHIQWAPLVBGQULAZPUTJSCISWPGMMUKEMYFWCKCWXCJOPQDJBPUWVYGDVMQAXAXNACQNRHOGHCCKJSSBTMPUBFWLEWNWJSDYCCKFAVUHDYYGNYADBPTFQMXJZPETHVSYSLHSTOBWGKDYEPRFNLZJEGOKJMXDZBXRLGTGHOAIDZVTJZQPQAEHKHRBFDLOCDQOAJEYACHXRXFMFYUODNVPIJZNYVIFHZWHRJBQKGTQANDUYZTGRRFQVQMLNJDJEXSTLVIMEPYPHEZMYYKHXGSCZBSRDHXHMEGFCOTHCDVETICYWEDVNXJGJLAAOQLANLGPEUUSIKQMWXSTYHOPEG"

{- Function that will print "No result!" if Maybe type contains Nothing, or the
 - contents of the "Just" part otherwise. -}
printMaybe :: (Show a) => Maybe a -> IO ()
printMaybe = maybe (putStrLn "No result!") print

{- This is the type of thing that will happen when testing your code. Note
 - that (1) You must have your code in a module called "Enigma". (2) The functions
 - encodeMessage, longestMenu and breakEnigma are expecting arguments in a
 - particular format. Make sure you write your types so that they are compatible.
 -
 - NOTE: The actual contents of the main function when testing your code will be
 - different to this. You SHOULD NOT submit this file, only Enigma.hs.
 -}
main = do
    print (encodeMessage "ILOVEHASKELL" enigma1)
    print (encodeMessage "ilovehaskell" enigma1)
    print (encodeMessage "iloveh*aske;ll" enigma1)
    print (encodeMessage "RILCDKIAEGGC" enigma1)
    -- putStrLn "First a test of encodeMessage: "
    -- print (encodeMessage "alice wasbe ginni ngtog etver ytire dofsi tting byher siste ronth ebank andof havin gnoth ingto doonc eortw icesh ehadp eeped intot heboo khers ister wasre ading butit hadno pictu resor conve rsati onsin itand whati stheu seofa bookt hough talic ewith outpi cture sorco nvers ation ssosh ewasc onsid ering inher ownmi ndasw ellas sheco uldfo rtheh otday madeh erfee lvery sleep yands tupid wheth erthe pleas ureof makin gadai sycha inwou ldbew ortht hetro ubleo fgett ingup andpi cking theda isies whens udden lyawh itera bbitw ithpi nkeye sranc loseb yhert herew asnot hings overy remar kable intha tnord idali cethi nkits overy mucho utoft heway tohea rther abbit sayto itsel fohde arohd earis hallb elate whens hetho ughti tover after wards itocc urred toher thats heoug httoh avewo ndere datth isbut atthe timei talls eemed quite natur albut whent herab bitac tuall ytook awatc houto fitsw aistc oatpo cketa ndloo kedat itand thenh urrie donal icest arted toher feetf oritf lashe dacro ssher mindt hatsh ehadn everb efore seena rabbi twith eithe rawai stcoa tpock etora watch totak eouto fitan dburn ingwi thcur iosit ysher anacr ossth efiel dafte ritan dfort unate lywas justi ntime tosee itpop downa large rabbi thole under thehe dgein anoth ermom entdo wnwen talic eafte ritne veron cecon sider ingho winth eworl dshew astog etout again thera bbith olewe ntstr aight onlik eatun nelfo rsome wayan dthen dippe dsudd enlyd ownso sudde nlyth atali cehad notam oment tothi nkabo utsto pping herse lfbef oresh efoun dhers elffa lling downa veryd eepwe lleit herth ewell wasve rydee porsh efell verys lowly forsh ehadp lenty oftim eassh ewent downt olook about heran dtowo nderw hatwa sgoin gtoha ppenn extfi rstsh etrie dtolo okdow nandm akeou twhat shewa scomi ngtob utitw astoo darkt oseea nythi ngthe nshel ooked atthe sides ofthe wella ndnot icedt hatth eywer efill edwit hcupb oards andbo okshe lvesh erean dther eshes awmap sandp ictur eshun gupon pegss hetoo kdown ajarf romon eofth eshel vesas shepa ssedi twasl abell edora ngema rmala debut toher great disap point menti twase mptys hedid notli ketod ropth ejarf orfea rofki lling someb odyun derne athso manag edtop utiti ntoon eofth ecupb oards asshe fellp astit " enigma1)
    -- -- print (encodeMessage "apples are nice sometimes" enigma1)
    -- putStrLn "And another test of encodeMessage: "
    -- print (encodeMessage "ntetm cniqr oyzky fpfws jtvzk mabit omo" enigma2)
    -- putStrLn "And another test of encodeMessage: "
    -- print (encodeMessage "Here is a test input string." enigma3)
    -- putStrLn "And another test of encodeMessage: "
    -- print (encodeMessage "Here is a test input string." enigma4)
    -- putStrLn "And another test of encodeMessage: "
    -- print (encodeMessage "Here is a test input string." enigma5)
    -- putStrLn "And another test of encodeMessage: "
    -- print (encodeMessage p1 testEnigma)
    -- putStrLn "cribxy: "
    -- print (longestMenu cribxy)

    -- putStrLn "cribb: "
    -- print (longestMenu cribb)
    -- putStrLn "Then a test of longestMenu: "
    -- print (longestMenu (zip p1 x1))

    -- putStrLn "Then a test of longestMenu: "
    -- print (longestMenu (zip p2 x2c))

    -- putStrLn "And now three tests of breakEnigma:"
    -- putStrLn "NOTE: Two of these most likely will return \"No result!\" after a very long search."
    -- putStrLn "You should get a result for at least one of them though."
    printMaybe (breakEnigma (zip z2 z1))
    printMaybe (breakEnigma (zip p2 x2b))
    printMaybe (breakEnigma (zip p2 x2c))
