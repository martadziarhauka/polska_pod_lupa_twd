# polska_pod_lupa_twd

## Roman Pirshtuk – zatrzymania osób poszukiwanych

### Wykres 1: „W święta poszukiwani rzadziej wpadają”
kody/01_accidents_research/01_download_data.ipynb,  
kody/01_accidents_research/02_prepare_data.R,  
kody/01_accidents_research/03_charts.R

Dane: Policja – „Informacja dzienna”, dzienna liczba zatrzymanych osób poszukiwanych w Polsce, sezony 2022/23–2025/26.  
https://www.policja.pl/pol/form/1,Informacja-dzienna.html

Co zrobiłem: pobrałem dzienne statystyki policji i wybrałem liczbę zatrzymanych osób poszukiwanych. Dla czterech sezonów zestawiłem okres od 15 grudnia do 2 stycznia, przypisując styczniowe dni do grudnia poprzedniego roku. Zachowałem liczby bezwzględne, a dni świąteczne wyróżniłem tłem i podpisami.

Dlaczego taki wykres: skumulowane słupki pokazują jednocześnie łączną liczbę zatrzymań danego dnia i wyniki poszczególnych sezonów. Dzięki temu widać świąteczny spadek. Kolory rozróżniają sezony, a zaznaczenie Wigilii, Bożego Narodzenia, Sylwestra i Nowego Roku ułatwia porównanie świąt z sąsiednimi dniami. Wykres pokazuje liczbę zatrzymań


## Marta Dziarhauka – wydatki i pogoda

### Wykres 2: „Czy święta są jeszcze białe?”
kody/03_weather/download_weather.R, kody/03_weather/weather.R

Dane: IMGW-PIB, dane synoptyczne dobowe, lata 1996–2026.
https://danepubliczne.imgw.pl/data/dane_pomiarowo_obserwacyjne/dane_meteorologiczne/dobowe/synop/

Co zrobiłam: skrypt download_weather.R pobiera archiwa IMGW i wybiera
pomiary pokrywy śnieżnej z 25.12 i 1.01. Pomiar wykonywany jest rano, więc
25.12 pokazuje śnieg w noc wigilijną, a 1.01 – w noc sylwestrową. Pominęłam
stacje wysokogórskie (Kasprowy Wierch, Śnieżka, Hala Gąsienicowa), na których
śnieg leży prawie zawsze. Za „białą” uznałam stację z co najmniej 1 cm śniegu
i policzyłam odsetek takich stacji w każdym sezonie oraz średnią dla każdej
dekady.

Dlaczego taki wykres: wykres gantelkowy pokazuje dwie rzeczy naraz:
ile śniegu było w danym sezonie i czy w ciągu tygodnia między Wigilią
a Nowym Rokiem śnieg spadł, czy stopniał. Średnie dla dekad wyraźnie pokazują
trend: odsetek białych stacji spada mniej więcej o połowę co 10 lat
(61% → 28% → 14%).

### Wykres 5: „Na co wydajemy więcej przed świętami?”
kody/02_retail/retail.R

Dane: GUS, Bank Danych Lokalnych – indeksy sprzedaży detalicznej
(poprzedni miesiąc = 100) dla 16 województw, lata 2021–2025.
https://bdl.stat.gov.pl/bdl/dane/podgrup/tablica

Co zrobiłam: wybrałam cztery kategorie towarów (książki, prasa i prezenty;
żywność i napoje; meble, RTV, AGD; paliwa) i usunęłam dane utajnione przez GUS.
Dla każdego miesiąca policzyłam medianę województw, a potem medianę z pięciu lat
oraz wartości minimalną i maksymalną. Od indeksu odjęłam 100, żeby otrzymać
zmianę sprzedaży w procentach.

Dlaczego taki wykres: słupki dla każdego miesiąca pokazują, że grudzień
wyraźnie odstaje od reszty roku, a wyróżnienie go kolorem pozwala od razu
porównać kategorie. Kreski min–max pokazują, że wzrost w grudniu powtarza się
co roku, a nie jest przypadkiem jednego roku. Osobne panele dla kategorii
pozwalają zestawić towary, które w grudniu rosną, z paliwami, które spadają.


## Mykola Ridkokasha – system elektroenergetyczny

### Wykres 3: „Jak gospodarka wygasza się na święta: profil obciążenia 23–25 grudnia”
kody/04_energy/energy.R

Dane: PSE (Polskie Sieci Elektroenergetyczne) / energy.instrat.pl – godzinowe zapotrzebowanie na moc w KSE (Krajowym Systemie Elektroenergetycznym).  
https://energy.instrat.pl/

Co zrobiłem: wczytałem dane o zapotrzebowaniu na moc w Polsce, wyciągając szczegółowy zakres od 23 do 25 grudnia. Przeliczyłem wartości z megawatów (MW) na gigawaty (GW). Dodatkowo w kodzie zidentyfikowałem punkt maksymalnego obciążenia w Wigilię, aby ustalić, o której dokładnie godzinie zaczyna się świąteczne wygaszanie gospodarki.

Dlaczego taki wykres: połączenie wykresu obszarowego (warstwowego) i liniowego pozwala prześledzić zmiany godzina po godzinie. Dzięki temu wyraźnie widać gwałtowny popołudniowy spadek zapotrzebowania w Wigilię. Oznaczenie szczytu punktem z etykietą zwraca uwagę na punkt zwrotny. Zastosowanie kolorów z palety (bordo dla Wigilii, szałwia i słońce dla tła oraz dni sąsiednich) pozwala łatwo odróżnić specyficzny dzień świąteczny od standardowego profilu.

### Paleta
Wspólne kolory wykresów są w kody/paleta.R: bordo oznacza Wigilię
i grudzień, złoto – Nowy Rok, szałwia – pozostałe dni i miesiące.
