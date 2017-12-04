(ns aoc.day1)

(def day1-data
  (str "73857646862514444739979151237829725363437326575178346717594627954612137"
       "82428342931896181695578996274321317419242359534783957372932953774336338"
       "11848896717272765186249883819531765428979755868345851112699621795332281"
       "72293723734558621778444784433918354845915252356518634648911779272449549"
       "25827786799436536592561374269299474738321293575385899438446558569241236"
       "27877977998358791243139547524479653888837328718692164742686623775673734"
       "27319767639594991499963155915847161221991832952774398729113713139245944"
       "86766479438544417416529743495114819825984524437367225234184772617942525"
       "95496113697687532518272575476837268453197261445513452359633835537444427"
       "35221153622387343831647781293766286214976629654567616317961783535996298"
       "87665939521892447361219479646483978798392716119793282717739524897385958"
       "27372677631815497767554628778987426533968875397718512933492971548638187"
       "52862785282476964641622976916981547127755895419452635748972665759964555"
       "47625537947927972497979333932115165151462742216327321116291372396585618"
       "66447571532129812233578926294228457132841456937546438644682488255191884"
       "31851958295473739154826875344329427783125427527983134346284982952166926"
       "46713137244198123219531693559848915834623825919191532658735422176965451"
       "74186966671487415849255644595485229916186865144812382582177536321924624"
       "45159463926862755455619893555739469247674422534653427539957647919279511"
       "58771231944177692469531494559697911176613943396258141822244578457498361"
       "35238151816658758334223381698932954441562112739799672399739721967648696"
       "66847296537635257686553244439911298621291812153399475552572795929212582"
       "46646215764736698583211625887436176149251356452358211458343439374688341"
       "11652972697243469732473452511419222964146422798658284547774174778767358"
       "88484397136193268896243269445533867828216335387753719159738999592952329"
       "27996742218926514374168947582441892731462993481877277714436887597223871"
       "88114969322892844242761166465577233347189373593241993783293795349592951"
       "48376638839384166443873428258366737337781194815144275124533576283966667"
       "91547531814844176342696362416842993761919369994779897357348334197721735"
       "231299249116477"))

(defn day1 [data]
  (->> (concat (drop 1 data) [(first data)])
       (map vector data)
       (filter (partial apply =))
       (map first)
       (map #(- (int %) 48))
       (apply +)))

(defn -main [& args]
  (println (day1 day1-data)))
