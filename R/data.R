#' EQ-5D-5L Crosswalk data
#'
#' Crosswalk index value calculation table to calculate EQ-5D-3L indices from EQ-5D-5L data
#'   for Denmark, France, Germany, Japan, Netherlands, Russia, Spain, Thailand, UK, USA
#'   and Zimbabwe.
#'
#' @source
#' van Hout B, Janssen MF, et al. Interim scoring for the EQ-5D-5L: Mapping the EQ-5D-5L
#'   to EQ-5D-3L value sets. Value in Health 2012 Jul-Aug;15(5):708-15. \doi{10.1016/j.jval.2012.02.008}.
#'   PMID: 22867780.
#'
#' Bailey H, Roudijk B, Brathwaite R. The EQ-5D-3L valuation study for 
#' Bermuda: using an on-line EQ-VT protocol. Eur J Health Econ. 2024 Jul 9. 
#' \doi{10.1007/s10198-024-01701-2}. Epub ahead of print. PMID: 38982011.
#' 
#' Al Rabayah A, Roudijk B, Purba FD, Rencz F, Jaddoua S, Siebert U. Valuation of the EQ-5D-3L 
#' in Jordan. Eur J Health Econ. 2024 Sep 3. \doi{10.1007/s10198-024-01712-z}. Epub ahead of print. 
#' PMID: 39225720.
#'
#' Omelyanovskiy V, Musina N, Ratushnyak S, Bezdenezhnykh T, Fediaeva V, Roudijk B,
#' Purba FD. Valuation of the EQ-5D-3L in Russia. Qual Life Res. 2021 Mar 13.
#' \doi{10.1007/s11136-021-02804-6}. Epub ahead of print. PMID: 33713323.
#'
#' \href{https://euroqol.org/information-and-support/resources/value-sets/}{EQ-5D-5L Crosswalk Index Value Sets}
#'
#' @name CW
#' @export
"CW"

#' DSU mapping from EQ-5D-3L to EQ-5D-5L
#'
#' Data for age and sex based mapping from EQ-5D-3L dimensions or utility index
#' score to EQ-5D-5L for China, Germany, Japan, Netherlands, South Korea, Spain
#' and UK.
#'
#' @source
#'
#' Hernández Alava M, Pudney S, Wailoo A. Estimating the Relationship Between EQ-5D-5L and EQ-5D-3L: Results from a UK Population Study. Pharmacoeconomics. 2023 Feb;41(2):199-207. \doi{10.1007/s40273-022-01218-7}. Epub 2022 Nov 30. PMID: 36449173.
#'
#' Hernández-Alava M, Pudney S. Econometric modelling of multiple self-reports of health states: The switch from EQ-5D-3L to EQ-5D-5L in evaluating drug therapies for rheumatoid arthritis. J Health Econ. 2017 Sep;55:139-152. \doi{10.1016/j.jhealeco.2017.06.013}. Epub 2017 Jul 4. PMID: 28778350.
#'
#' \href{https://www.sheffield.ac.uk/nice-dsu/methods-development/mapping-eq-5d-5l-3l}{NICE DSU mapping website}.
#'
#' @name DSU3L
#' @export
"DSU3L"

#' DSU mapping from EQ-5D-5L to EQ-5D-3L
#'
#' Data for age and sex based mapping from EQ-5D-5L dimensions or utility index
#' score to EQ-5D-3L for China, Germany, Japan, Netherlands, South Korea, Spain
#' and UK.
#'
#' @source
#' 
#' Hernández Alava M, Pudney S, Wailoo A. Estimating the Relationship Between EQ-5D-5L and EQ-5D-3L: Results from a UK Population Study. Pharmacoeconomics. 2023 Feb;41(2):199-207. \doi{10.1007/s40273-022-01218-7}. Epub 2022 Nov 30. PMID: 36449173.
#'
#' Hernández-Alava M, Pudney S. Econometric modelling of multiple self-reports of health states: The switch from EQ-5D-3L to EQ-5D-5L in evaluating drug therapies for rheumatoid arthritis. J Health Econ. 2017 Sep;55:139-152. \doi{10.1016/j.jhealeco.2017.06.013}. Epub 2017 Jul 4. PMID: 28778350.
#'
#' \href{https://www.sheffield.ac.uk/nice-dsu/methods-development/mapping-eq-5d-5l-3l}{NICE DSU mapping website}.
#'
#' @name DSU5L
#' @export
"DSU5L"

#' EQ-5D-3L Reverse Crosswalk data (deprecated)
#'
#' Reverse Crosswalk index value table to calculate EQ-5D-5L indices from EQ-5D-3L data
#'   for England, Germany, Netherlands and USA. Table uses the values published 
#'   on the EuroQol analysis tools webpage based on reverse engineering of
#'   van Hout et al (2012)
#'
#' @source
#' \href{https://euroqol.org/information-and-support/resources/value-sets/}{Reverse crosswalk datasets}
#'
#' @name RCW
#' @export
"RCW"

#' EQ-5D-3L Reverse Crosswalk data
#'
#' Reverse Crosswalk index value table to calculate EQ-5D-5L indices from EQ-5D-3L data
#'   using the van Hout et al (2021) method.
#'
#' @source
#' \doi{10.1016/j.jval.2021.03.009}
#'
#' @name RCWVH
#' @export
"RCWVH"

#' EQ-5D-3L TTO value set data
#'
#' Coefficients for the estimation of the EQ-5D-3L index values based on TTO valuation studies
#'   for Argentina, Australia, Brazil, Canada, Chile, China, Denmark, Ecuador, 
#'   France, Germany, Hungary, Italy, Japan, Netherlands, Pakistan, Poland, Portugal, 
#'   Russia, Singapore, SouthKorea, Spain, SriLanka, Sweden, Taiwan, Thailand, 
#'   Trinidad and Tobago, Tunisia, UK, USA and Zimbabwe.
#'
#' @source
#' Szende, A., Oppe, M., & de Charro, F. (2007), Comparative review of Time Trade-Off value sets.
#'   In Szende, A., Oppe, M., & Devlin, N. (Ed.), EQ-5D Value Sets: Inventory, Comparative Review
#'   and User Guide (pp. 27-28). Dordrecht, The Netherlands: Springer.
#'
#' Janssen, B., Szende, A., & Ramos-Goñi JM. (2014), Data and Methods.
#'   Szende, A., Janssen, B., & Cabasés, J. (Ed.), In Self-Reported Population Health: An
#'   International Perspective based on EQ-5D (p 13). Dordrecht, The Netherlands: Springer.
#' \itemize{
#' \item{\strong{Argentina}: Augustovski FA, Irazola VE, Velazquez AP, Gibbons L, Craig BM. Argentine valuation of the EQ-5D health states. Value Health. 2009 Jun;12(4):587-96. \doi{10.1111/j.1524-4733.2008.00468.x}. Epub 2008 Nov 12. PMID: 19900257.}
#' \item{\strong{Australia}: Viney R, Norman R, King MT, Cronin P, Street DJ, Knox S, Ratcliffe J. Time trade-off derived EQ-5D weights for Australia. Value Health. 2011 Sep-Oct;14(6):928-36. \doi{10.1016/j.jval.2011.04.009}. PMID: 21914515.}
#' \item{\strong{Bermuda}: Bailey H, Roudijk B, Brathwaite R. The EQ-5D-3L valuation study for Bermuda: using an on-line EQ-VT protocol. Eur J Health Econ. 2024 Jul 9. \doi{10.1007/s10198-024-01701-2}. Epub ahead of print. PMID: 38982011.}
#' \item{\strong{Brazil}: Viegas Andrade M, Noronha K, Kind P, Maia AC, Miranda de Menezes R, De Barros Reis C, Nepomuceno Souza M, Martins D, Gomes L, Nichele D, Calazans J, Mascarenhas T, Carvalho L, Lins C. Societal Preferences for EQ-5D Health States from a Brazilian Population Survey. Value Health Reg Issues. 2013 Dec;2(3):405-412. doi: 10.1016/j.vhri.2013.01.009. Epub 2013 Mar 13. Erratum in: Value Health Reg Issues. 2016 Dec;11:85-87. \doi{10.1016/j.vhri.2016.12.001}. PMID: 29702778.}
#' \item{\strong{Canada}: Bansback N, Tsuchiya A, Brazier J, Anis A. Canadian valuation of EQ-5D health states: preliminary value set and considerations for future valuation studies. PLoS One. 2012;7(2):e31115. \doi{10.1371/journal.pone.0031115}. Epub 2012 Feb 6. PMID: 22328929.}
#' \item{\strong{Chile}: Zarate V, Kind P, Valenzuela P, Vignau A, Olivares-Tirado P, Munoz A. Social valuation of EQ-5D health states: the Chilean case. Value Health. 2011 Dec;14(8):1135-41. \doi{10.1016/j.jval.2011.09.002}. Epub 2011 Nov 6. PMID: 22152184.}
#' \item{\strong{China}: Liu GG, Wu H, Li M, Gao C, Luo N. Chinese time trade-off values for EQ-5D health states. Value Health. 2014 Jul;17(5):597-604. \doi{10.1016/j.jval.2014.05.007}. Epub 2014 Jul 23. PMID: 25128053.}
#' \item{\strong{Denmark}: Wittrup-Jensen KU, Lauridsen J, Gudex C, Pedersen KM. Generation of a Danish TTO value set for EQ-5D health states. Scand J Public Health. 2009 Jul;37(5):459-66. \doi{10.1177/1403494809105287}. Epub 2009 May 1. PMID: 19411320.}
#' \item{\strong{Ecuador}: Lucio R, Flores V,  Granja M, Mata G. Resultados de la encuesta de valoración social de los estados de salud de lAños de vida ajustados por calidad (QALY'S). 2019. \href{https://www.researchgate.net/publication/335840590_QALYs_FINAL_Ecuadordig}{Link}}
#' \item{\strong{France}: Chevalier J, de Pouvourville G. Valuing EQ-5D using time trade-off in France. Eur J Health Econ. 2013 Feb;14(1):57-66. \doi{10.1007/s10198-011-0351-x}. Epub 2011 Sep 21. PMID: 21935715.}
#' \item{\strong{Germany}: Greiner W, Claes C, Busschbach JJ, von der Schulenburg JM. Validating the EQ-5D with time trade off for the German population. Eur J Health Econ. 2005 Jun;6(2):124-30. \doi{10.1007/s10198-004-0264-z}. PMID: 19787848.}
#' \item{\strong{Hungary}: Rencz F, Brodszky V, Gulácsi L, Golicki D, Ruzsa G, Pickard AS, Law EH, Péntek M. Parallel Valuation of the EQ-5D-3L and EQ-5D-5L by Time Trade-Off in Hungary. Value Health. 2020 Sep;23(9):1235-1245. \doi{10.1016/j.jval.2020.03.019}. Epub 2020 Aug 12. PMID: 32940242.}
#' \item{\strong{Italy}: Scalone L, Cortesi PA, Ciampichini R, Belisari A, D'Angiolella LS, Cesana G, Mantovani LG. Italian population-based values of EQ-5D health states. Value Health. 2013 Jul-Aug;16(5):814-22. \doi{10.1016/j.jval.2013.04.008}. Epub 2013 Jun 19. PMID: 23947975.}
#' \item{\strong{Japan}: Tsuchiya A, Ikeda S, Ikegami N, Nishimura S, Sakai I, Fukuda T, Hamashima C, Hisashige A, Tamura M. Estimating an EQ-5D population value set: the case of Japan. Health Econ. 2002 Jun;11(4):341-53. \doi{10.1002/hec.673}. PMID: 12007165.}
#' \item{\strong{Jordan}: Al Rabayah A, Roudijk B, Purba FD, Rencz F, Jaddoua S, Siebert U. Valuation of the EQ-5D-3L in Jordan. Eur J Health Econ. 2024 Sep 3. \doi{10.1007/s10198-024-01712-z}. Epub ahead of print. PMID: 39225720.}
#' \item{\strong{Netherlands}: Lamers LM, McDonnell J, Stalmeier PF, Krabbe PF, Busschbach JJ. The Dutch tariff: results and arguments for an effective design for national EQ-5D valuation studies. Health Econ. 2006 Oct;15(10):1121-32. \doi{10.1002/hec.1124}. PMID: 16786549.}
#' \item{\strong{Pakistan}: Malik M, Gu NY, Hussain A, Roudijk B, Purba FD. The EQ-5D-3L Valuation Study in Pakistan. Pharmacoecon Open. 2023 Sep 13. \doi{10.1007/s41669-023-00437-8}. Epub ahead of print. PMID: 37702988.}
#' \item{\strong{Poland}: Golicki D, Jakubczyk M, Niewada M, Wrona W, Busschbach JJ. Valuation of EQ-5D health states in Poland: first TTO-based social value set in Central and Eastern Europe. Value Health. 2010 Mar-Apr;13(2):289-97. \doi{10.1111/j.1524-4733.2009.00596.x}. Epub 2009 Sep 10. PMID: 19744296.}
#' \item{\strong{Portugal}: Ferreira LN, Ferreira PL, Pereira LN, Oppe M. The valuation of the EQ-5D in Portugal. Qual Life Res. 2014 Mar;23(2):413-23. \doi{10.1007/s11136-013-0448-z}. Epub 2013 Jun 8. PMID: 23748906.}
#' \item{\strong{Russia}: Omelyanovskiy V, Musina N, Ratushnyak S, Bezdenezhnykh T, Fediaeva V, Roudijk B, Purba FD. Valuation of the EQ-5D-3L in Russia. Qual Life Res. 2021 Mar 13. \doi{10.1007/s11136-021-02804-6}. Epub ahead of print. PMID: 33713323.}
#' \item{\strong{Singapore}: Luo N, Wang P, Thumboo J, Lim YW, Vrijhoef HJ. Valuation of EQ-5D-3L health states in Singapore: modeling of time trade-off values for 80 empirically observed health states. Pharmacoeconomics. 2014 May;32(5):495-507. \doi{10.1007/s40273-014-0142-1}. PMID: 24519603.}
#' \item{\strong{Spain}: Badia X, Roset M, Herdman M, Kind P. A comparison of United Kingdom and Spanish general population time trade-off values for EQ-5D health states. Med Decis Making. 2001 Jan-Feb;21(1):7-16. \doi{10.1177/0272989X0102100102}. PMID: 11206949.}
#' \item{\strong{South Korea}: Lee YK, Nam HS, Chuang LH, Kim KY, Yang HK, Kwon IS, Kind P, Kweon SS, Kim YT. South Korean time trade-off values for EQ-5D health states: modeling with observed values for 101 health states. Value Health. 2009 Nov-Dec;12(8):1187-93. \doi{10.1111/j.1524-4733.2009.00579.x}. Epub 2009 Jul 29. PMID: 19659703.}
#' \item{\strong{Sri Lanka}: Kularatna S, Whitty JA, Johnson NW, Jayasinghe R, Scuffham PA. Valuing EQ-5D health states for Sri Lanka. Qual Life Res. 2015 Jul;24(7):1785-93. \doi{10.1007/s11136-014-0906-2}. Epub 2014 Dec 28. PubMed PMID: PMID: 25543271.}
#' \item{\strong{Sweden}: Burström K, Sun S, Gerdtham UG, Henriksson M, Johannesson M, Levin LÅ, Zethraeus N. Swedish experience-based value sets for EQ-5D health states. Qual Life Res. 2014 Mar;23(2):431-42. \doi{10.1007/s11136-013-0496-4}. PMID: 23975375.}
#' \item{\strong{Taiwan}: Lee HY, Hung MC, Hu FC, Chang YY, Hsieh CL, Wang JD. Estimating quality weights for EQ-5D (EuroQol-5 dimensions) health states with the time trade-off method in Taiwan. J Formos Med Assoc. 2013 Nov;112(11):699-706. \doi{10.1016/j.jfma.2012.12.015}. Epub 2013 Feb 12. PMID: 24183199.}
#' \item{\strong{Thailand}: Tongsiri S, Cairns J. Estimating population-based values for EQ-5D health states in Thailand. Value Health. 2011 Dec;14(8):1142-5. \doi{10.1016/j.jval.2011.06.005}. PMID: 22152185.}
#' \item{\strong{Trinidad and Tobago}: Bailey H, Stolk E, Kind P. Toward Explicit Prioritization for the Caribbean: An EQ-5D Value Set for Trinidad and Tobago. Value Health Reg Issues. 2016 Dec;11:60-67. \doi{10.1016/j.vhri.2016.07.010}. PMID: 27986200.}
#' \item{\strong{Tunisia}: Chemli J, Drira C, Felfel H, Roudijk B, Al Sayah F, Kouki M, Kooli A, Razgallah Khrouf M. Valuing health-related quality of life using a hybrid approach: Tunisian value set for the EQ-5D-3L. Qual Life Res. 2021 Jan 14. \doi{10.1007/s11136-020-02730-z}. Epub ahead of print. PMID: 33447958.}
#' \item{\strong{UK}: Dolan P. Modeling valuations for EuroQol health states. Med Care. 1997 Nov;35(11):1095-108. \doi{10.1097/00005650-199711000-00002}. PMID: 9366889.}
#' \item{\strong{USA}: Shaw JW, Johnson JA, Coons SJ. US valuation of the EQ-5D health states: development and testing of the D1 valuation model. Med Care. 2005 Mar;43(3):203-20. \doi{10.1097/00005650-200503000-00003}. PMID: 15725977.}
#' \item{\strong{Zimbabwe}: Jelsma J, Hansen K, De Weerdt W, De Cock P, Kind P. How do Zimbabweans value health states? Popul Health Metr. 2003 Dec 16;1(1):11. \doi{10.1186/1478-7954-1-11}. PMID: 14678566.}
#' }
#'
#' @name TTO
#' @export
"TTO"

#' EQ-5D-3L VAS value set data
#'
#' Coefficients for the estimation of the EQ-5D-3L index values based on VAS valuation studies
#'  for Belgium, Denmark, Europe, Finland, Germany, Iran, Malaysia, New Zealand,
#'  Slovenia, Spain and UK.
#'
#' @source
#' Oppe, M., Szende, A., & de Charro, F. (2007), Comparative review of Visual Analogue Scale value sets.
#'   In Szende, A., Oppe, M., & Devlin, N. (Ed.), EQ-5D Value Sets: Inventory, Comparative Review
#'   and User Guide (pp. 37-38). Dordrecht, The Netherlands: Springer.
#'
#' \itemize{
#' \item{\strong{Belgium}: Cleemput I. A social preference valuations set for EQ-5D health states in Flanders, Belgium. Eur J Health Econ. 2010 Apr;11(2):205-13. \doi{10.1007/s10198-009-0167-0}. Epub 2009 Jul 7. PMID: 19582490.}
#' \item{\strong{Denmark}: Wittrup-Jensen KU, Lauridsen JT, Gudex C, Brooks R, Pedersen KM. Estimating Danish EQ-5D tariffs using TTO and VAS. In: Norinder A, Pedersen K, Roos P, editors. Proceedings of the 18th Plenary Meeting of the EuroQol Group. 2001. Copenhagen, Denmark. IHE, The Swedish Institute for Health Economics, 2002: 257-292.}
#' \item{\strong{Europe}: Greiner W, Weijnen T, Nieuwenhuizen M, et al. A single European currency for EQ-5D health states. Results from a six country study. Eur J Health Econ 2003; 4(3):222-231.}
#' \item{\strong{Finland}: Ohinmaa, A., & Sintonen, H. (1998, October). Inconsistencies and modelling of the Finnish EuroQol (EQ-5D) preference values. In EuroQol Plenary Meeting (pp. 1-2). Health Economics and Health System Research, University of Hannover.}
#' \item{\strong{Germany}: Claes, C., Greiner, W., Uber, A., & Graf von der Schulenburg, J. M. (1999). An interview-based comparison of the TTO and VAS values given to EuroQol states of health by the general German population. In Proceedings of the 15th Plenary Meeting of the EuroQol Group. Hannover, Germany: Centre for Health Economics and Health Systems Research, University of Hannover (pp. 13-38).}
#' \item{\strong{Iran}: Goudarzi R, Zeraati H, Akbari Sari A, Rashidian A, Mohammad K. Population-Based Preference Weights for the EQ-5D Health States Using the Visual Analogue Scale (VAS) in Iran. Iran Red Crescent Med J. 2016 Feb 13;18(2):e21584. \doi{10.5812/ircmj.21584}. PMID: 27186384.}
#' \item{\strong{Malaysia}: Yusof FA, Goh A, Azmi S. Estimating an EQ-5D value set for Malaysia using time trade-off and visual analogue scale methods. Value Health. 2012 Jan-Feb;15(1 Suppl):S85-90. \doi{10.1016/j.jval.2011.11.024}. PMID: 22265073.}
#' \item{\strong{New Zealand}: Devlin NJ, Hansen P, Kind P, Williams A. Logical inconsistencies in survey respondents' health state valuations -- a methodological challenge for estimating social tariffs. Health Econ. 2003 Jul;12(7):529-44. \doi{10.1002/hec.741}. PMID: 12825206.}
#' \item{\strong{Slovenia}: Prevolnik Rupel V, Rebolj M. The Slovenian VAS Tariff based on valuations of EQ-5D health states from the general population. In: Cabasés JM, Gaminde I, editors. Proceedings of the 17th Plenary Meeting of the EuroQol Group. Universidad Pública de Navarra 2001; 23-47.}
#' \item{\strong{Spain} Badia X, Roset M, Monserrat S, Herdman M. The Spanish VAS tariff based on valuation of EQ-5D health states from the general population. In: Rabin RE et al, editors. EuroQol Plenary meeting Rotterdam 1997, 2-3 October. Discussion papers. Centre for Health Policy & Law, Erasmus University, Rotterdam, 1998; 93-114}
#' \item{\strong{UK} MVH Group. The Measurement and Valuation of Health. Final report on the modeling of valuation tariffs. York Centre for Health Economics, 1995.}
#' }
#'
#' @name VAS
#' @export
"VAS"

#' EQ-5D-5L VT value set data
#'
#' EQ-5D-5L VT value set calculation data for Australia, Belgium, Canada, China, 
#' Denmark, Egypt, England, Ethiopia, France, Germany, Ghana, HongKong, Hungary,
#' India, Indonesia, Iran, Ireland, Italy, Japan, Malaysia, Mexico, Morocco, 
#' Netherlands, NewZealand, Norway, Peru, Philippines, Poland, Portugal, 
#' Romania, SaudiArabia, Slovenia, SouthKorea, Spain, Sweden, Taiwan, Thailand, 
#' Uganda, UAE, Uruguay, USA, Vietnam and Western Preference Pattern (WePP).
#'
#' @source
#' \itemize{
#' \item{\strong{Australia}: Norman R, Mulhern B, Lancsar E, Lorgelly P, Ratcliffe J, Street D, Viney R. The Use of a Discrete Choice Experiment Including Both Duration and Dead for the Development of an EQ-5D-5L Value Set for Australia. Pharmacoeconomics. 2023 Jan 31. \doi{10.1007/s40273-023-01243-0}. Epub ahead of print. PMID: 36720793.}
#' \item{\strong{Belgium}: Bouckaert N, Cleemput I, Devriese S, Gerkens S. An EQ-5D-5L Value Set for Belgium. Pharmacoecon Open. 2022 Aug 4. \doi{10.1007/s41669-022-00353-3}. Epub ahead of print. PMID: 35927410.}
#' \item{\strong{Canada}: Xie F, Pullenayegum E, Gaebel K, Bansback N, Bryan S, Ohinmaa A, Poissant L, Johnson JA; Canadian EQ-5D-5L Valuation Study Group. A Time Trade-off-derived Value Set of the EQ-5D-5L for Canada. Med Care. 2016 Jan;54(1):98-105. \doi{10.1097/MLR.0000000000000447}. PMID: 26492214.}
#' \item{\strong{China}: Luo N, Liu G, Li M, Guan H, Jin X, Rand-Hendriksen K. Estimating an EQ-5D-5L Value Set for China. Value Health. 2017 Apr;20(4):662-669. \doi{10.1016/j.jval.2016.11.016}. Epub 2017 Feb 9. PMID: 28408009.}
#' \item{\strong{Denmark}: Jensen CE, Sørensen SS, Gudex C, Jensen MB, Pedersen KM, Ehlers LH. The Danish EQ-5D-5L Value Set: A Hybrid Model Using cTTO and DCE Data. Appl Health Econ Health Policy. 2021 Feb 2. \doi{10.1007/s40258-021-00639-3}. Epub ahead of print. PMID: 33527304.}
#' \item{\strong{Egypt}: Al Shabasy S, Abbassi M, Finch A, Roudijk B, Baines D, Farid S. The EQ-5D-5L Valuation Study in Egypt. Pharmacoeconomics. 2021 Nov 17:1–15. \doi{10.1007/s40273-021-01100-y}. Epub ahead of print. PMID: 34786590.}
#' \item{\strong{England}: Devlin NJ, Shah KK, Feng Y, Mulhern B, van Hout B. Valuing health-related quality of life: An EQ-5D-5L value set for England. Health Econ. 2018 Jan;27(1):7-22. \doi{10.1002/hec.3564}. Epub 2017 Aug 22. PMID: 28833869.}
#' \item{\strong{Ethiopia}: Welie AG, Gebretekle GB, Stolk E, Mukuria C, Krahn MD, Enquoselassie F, Fenta TG. Valuing Health State: An EQ-5D-5L Value Set for Ethiopians. Value Health Reg Issues. 2019 Nov 1;22:7-14. \doi{10.1016/j.vhri.2019.08.475}. PMID: 31683254.}
#' \item{\strong{France}: Andrade LF, Ludwig K, Goni JMR, Oppe M, de Pouvourville G. A French Value Set for the EQ-5D-5L. Pharmacoeconomics. 2020 Jan 8. \doi{10.1007/s40273-019-00876-4}. PMID: 31912325.}
#' \item{\strong{Germany}: Ludwig K, Graf von der Schulenburg JM, Greiner W. German Value Set for the EQ-5D-5L. Pharmacoeconomics. 2018 Feb;36(6):663-674. \doi{10.1007/s40273-018-0615-8}. PMID: 29460066.}
#' \item{\strong{Ghana}: Addo R, Mulhern B, Norman R, Owusu R, Viney R, Nonvignon J. An EQ-5D-5L Value Set for Ghana Using an Adapted EuroQol Valuation Technology Protocol. Value Health Reg Issues. 2024 Sep 4;45:101045. \doi{10.1016/j.vhri.2024.101045}. Epub ahead of print. PMID: 39236574.}
#' \item{\strong{HongKong}: Wong ELY, Ramos-Goñi JM, Cheung AWL, Wong AYK, Rivero-Arias O. Assessing the Use of a Feedback Module to Model EQ-5D-5L Health States Values in Hong Kong. Patient. 2018 Apr;11(2):235-247. \doi{10.1007/s40271-017-0278-0}. PMID: 29019161.}
#' \item{\strong{Hungary}: Rencz F, Brodszky V, Gulácsi L, Golicki D, Ruzsa G, Pickard AS, Law EH, Péntek M. Parallel Valuation of the EQ-5D-3L and EQ-5D-5L by Time Trade-Off in Hungary. Value Health. 2020 Sep;23(9):1235-1245. \doi{10.1016/j.jval.2020.03.019}. Epub 2020 Aug 12. PMID: 32940242.}
#' \item{\strong{India}: Jyani G, Sharma A, Prinja S, Kar SS, Trivedi M, Patro BK, Goyal A, Purba FD, Finch AP, Rajsekar K, Raman S, Stolk E, Kaur M. Development of an EQ-5D Value Set for India Using an Extended Design (DEVINE) Study: The Indian 5-Level Version EQ-5D Value Set. Value Health. 2022 Jul;25(7):1218-1226. \doi{10.1016/j.jval.2021.11.1370}. Epub 2022 Jan 5. PMID: 35779943.}
#' \item{\strong{Indonesia}: Purba FD, Hunfeld JAM, Iskandarsyah A, Fitriana TS, Sadarjoen SS, Ramos-Goñi JM, Passchier J, Busschbach JJ. The Indonesian EQ-5D-5L Value Set. PharmacoEconomics. 2017 Nov;35(11)1153-1165. \doi{10.1007/s40273-017-0538-9}. PMID: 28695543.}
#' \item{\strong{Iran}: Afshari S, Daroudi R, Goudarzi R, Mahboub-Ahari A, Yaseri M, Sari AA, Ameri H, Bahariniya S, Oliaei-Manesh A, Kalavani K, Zare Z, Hasannezhad E, Mirzaei M, Amiri Z. A national survey of Iranian general population to estimate a value set for the EQ-5D-5L. Qual Life Res. 2023 Mar 10. \doi{10.1007/s11136-023-03378-1}. Epub ahead of print. PMID: 36897530.}
#' \item{\strong{Ireland}: Hobbins A, Barry L, Kelleher D, Shah K, Devlin N, Ramos Goñi JM, O’Neill C. Utility Values for Health States in Ireland: A Value Set for the EQ-5D-5L. PharmacoEconomics. 2018 Nov;36(11):1345-1353. \doi{10.1007/s40273-018-0690-x}. PMID: 30051267.}
#' \item{\strong{Italy}: Finch AP, Meregaglia M, Ciani O, Roudijk B, Jommi C. An EQ-5D-5L value set for Italy using videoconferencing interviews and feasibility of a new mode of administration. Soc Sci Med. 2022 Jan;292:114519. \doi{10.1016/j.socscimed.2021.114519}. Epub 2021 Oct 28. PMID: 34736804.}
#' \item{\strong{Japan}: Shiroiwa T, Ikeda S, Noto S, Igarashi A, Fukuda T, Saito S, Shimozuma K. Comparison of Value Set Based on DCE and/or TTO Data: Scoring for EQ-5D-5L Health States in Japan. Value Health. 2016 Jul-Aug;19(5):648-54. \doi{10.1016/j.jval.2016.03.1834}. Epub 2016 Apr 26. PMID: 27565282.}
#' \item{\strong{Malaysia}: Shafie AA; Vasan Thakumar A; Lim CJ;Luo N; Rand-Hendriksen K; Yusof FA. EQ-5D-5L Valuation for the Malaysian Population. PharmacoEconomics. 2019 May;37(5):715-725. \doi{10.1007/s40273-018-0758-7}. PMID: 30535779.}
#' \item{\strong{Mexico}: Gutierrez-Delgado C, Galindo-Suárez RM, Cruz-Santiago C, Shah K, Papadimitropoulos M, Feng Y, Zamora B, Devlin N. EQ-5D-5L Health-State Values for the Mexican Population. Appl Health Econ Health Policy. 2021 Nov;19(6):905-914. \doi{10.1007/s40258-021-00658-0}. Epub 2021 Jun 26. PMID: 34173957.}
#' \item{\strong{Morocco}: Azizi A, Boutib A, Achak D, Purba FD, Rencz F, Saad E, Hilali A, Ahid S, Nejjari C, Stolk EA, Roudijk B, Youlyouz-Marfak I, Marfak A.Valuing health-related quality of life: an EQ-5D-5L value set for Morocco. Qual Life Res. 2025 Feb 28. \doi{10.1007/s11136-025-03930-1}. Online ahead of print. PMID: 40019677.}
#' \item{\strong{Netherlands}: Versteegh MM, Vermeulen KM, Evers SM, de Wit GA, Prenger R, Stolk EA. Dutch Tariff for the Five-Level Version of EQ-5D. Value in Health. 2016 Jun;19(4):343-52.  \doi{10.1016/j.jval.2016.01.003}. PMID: 27325326.}
#' \item{\strong{New Zealand}: Sullivan T, Hansen P, Ombler F, Derrett S, Devlin N. A new tool for creating personal and social EQ-5D-5L value sets, including valuing 'dead'. Soc Sci Med. 2020 Feb;246:112707. \doi{10.1016/j.socscimed.2019.112707}. Epub 2019 Nov 30. PMID: 31945596.}
#' \item{\strong{Norway}: Garratt AM, Stavem K, Shaw JW, Rand K. EQ-5D-5L value set for Norway: a hybrid model using cTTO and DCE data. Qual Life Res. 2024 Nov 20. \doi{10.1007/s11136-024-03837-3}. Epub ahead of print. PMID: 39565555.}
#' \item{\strong{Peru} Augustovski F, Belizán M, Gibbons L, Reyes N, Stolk E, Craig BM, Tejada RA. Peruvian Valuation of the EQ-5D-5L: A Direct Comparison of Time Trade-Off and Discrete Choice Experiments. Value Health. 2020;23(7):880-888. \doi{10.1016/j.jval.2020.05.004}. PMID: 32762989.}
#' \item{\strong{Philippines} Miguel RTD, Rivera AS, Cheng KJG, Rand K, Purba FD, Luo N, Zarsuelo MA, Genuino-Marfori AJ, Florentino-Fariñas I, Guerrero AM, Lam HY. Estimating the EQ-5D-5L value set for the Philippines. Qual Life Res. 2022 May 9. \doi{10.1007/s11136-022-03143-w}. PMID: 35532835.}
#' \item{\strong{Poland} Golicki D, Jakubczyk M, Niewada M, Wrona W, Busschbach JJ. Valuation of EQ-5D health states in Poland: first TTO-based social value set in Central and Eastern Europe. Value Health. 2010 Mar-Apr;13(2):289-97. \doi{10.1111/j.1524-4733.2009.00596.x}. PMID: 19744296.}
#' \item{\strong{Portugal} Ferreira PL, Antunes P, Ferreira LN, Pereira LN, Ramos-Goñi JM. A hybrid modelling approach for eliciting health state preferences: the Portuguese EQ-5D-5L value set. Qual Life Res. 2019 Jun 14. \doi{10.1007/s11136-019-02226-5}. PMID: 31201730.}
#' \item{\strong{Romania} Olariu E, Mohammed W, Oluboyede Y, Caplescu R, Niculescu-Aron IG, Paveliu MS, Vale L. EQ-5D-5L: a value set for Romania. Eur J Health Econ. 2022 Jun 10. \doi{10.1007/s10198-022-01481-7}. Epub ahead of print. PMID: 35688994.}
#' \item{\strong{Saudi Arabia} Al-Jedai A, Almudaiheem H, Al-Salamah T, Aldosari M, Almutairi AR, Almogbel Y, AlRuthia Y, Althemery AU, Alluhidan M, Roudijk B, Purba FD, Awad N, O'jeil R. Valuation of EQ-5D-5L in the Kingdom of Saudi Arabia: A national representative study. Value Health. 2024 Feb 9:S1098-3015(24)00047-0. \doi{10.1016/j.jval.2024.01.017}. PMID: 38342365.}
#' \item{\strong{Slovenia} Prevolnik Rupel V, Ogorevc M. EQ-5D-5L Value Set for Slovenia. Pharmacoeconomics. 2023 Jun 21. \doi{10.1007/s40273-023-01280-9}. Epub ahead of print. PMID: 37341959.}
#' \item{\strong{South Korea} Kim SH, Ahn J, Ock M, Shin S, Park J, Luo N, Jo MW. The EQ-5D-5L valuation study in Korea. Qual Life Res. 2016 Jul;25(7):1845-52. \doi{10.1007/s11136-015-1205-2}. PMID: 26961008.}
#' \item{\strong{Spain}: Ramos-Goñi JM, Craig B, Oppe M, Ramallo-Fariña Y, Pinto-Prades JL, Luo N, Rivero-Arias O. Handling data quality issues to estimate the Spanish EQ-5D-5L Value Set using a hybrid interval regression approach.  Value in Health 2018 May;21(5):596-604. \doi{10.1016/j.jval.2017.10.023}. PMID: 29753358.}
#' \item{\strong{Sweden (2020)}: Burström K, Teni FS, Gerdtham UG, Leidl R, Helgesson G, Rolfson O, Henriksson M. Experience-Based Swedish TTO and VAS Value Sets for EQ-5D-5L Health States. Pharmacoeconomics. 2020 Apr 20. \doi{10.1007/s40273-020-00905-7}. PMID: 32307663.}
#' \item{\strong{Sweden (2022)}: Sun S, Chuang LH, Sahlén KG, Lindholm L, Norström F. Estimating a social value set for EQ-5D-5L in Sweden. Health Qual Life Outcomes. 2022 Dec 23;20(1):167. \doi{10.1186/s12955-022-02083-w}. PMID: 36564844.}
#' \item{\strong{Taiwan}: Lin HW, Li CI, Lin FJ, Chang JY, Gau CS, Luo N, Pickard AS, Ramos Goñi JM, Tang CH, Hsu CN. Valuation of the EQ-5D-5L in Taiwan. PLoS One. 2018; 13(12):: e0209344. \doi{10.1371/journal.pone.0209344}. PMID: 30586400.}
#' \item{\strong{Thailand} Pattanaphesaj J, Thavorncharoensap M, Ramos-Goñi JM, Tongsiri S, Ingsrisawang L, Teerawattananon Y. The EQ-5D-5L Valuation study in Thailand. Expert Review of Pharmacoeconomics & Outcomes Research. 2018 Oct;18(5):551-558. \doi{10.1080/14737167.2018.1494574}. PMID: 29958008.}
#' \item{\strong{Trinidad_and_Tobago} Bailey H, Jonker MF, Pullenayegum E, Rencz F, Roudijk B. The EQ-5D-5L valuation study for Trinidad and Tobago. Health Qual Life Outcomes. 2024 Jul 2;22(1):51. \doi{10.1186/s12955-024-02266-7}. PMID: 38956543.}
#' \item{\strong{UAE} Al Sayah F, Roudijk B, El Sadig M, Al Mannaei A, Farghaly MN, Dallal S, Kaddoura R, Metni M, Elbarazi I, Kharroubi SA. A Value Set for the EQ-5D-5L for United Arab Emirates. Value Health. 2025 Jan 27:S1098-3015(25)00021-X. \doi{10.1016/j.jval.2025.01.003}. Epub ahead of print. PMID: 39880198.}
#' \item{\strong{Uganda} Yang F, Katumba KR, Roudijk B, Yang Z, Revill P, Griffin S, Ochanda PN, Lamorde M, Greco G, Seeley J, Sculpher M. Developing the EQ-5D-5L Value Set for Uganda Using the 'Lite' Protocol. Pharmacoeconomics. 2021 Nov 29:1–13. \doi{10.1007/s40273-021-01101-x}. PMID: 34841471.}
#' \item{\strong{Uruguay}: Augustovski F, Rey-Ares L, Irazola V, Garay OU, Gianneo O, Fernández G, Morales M, Gibbons L, Ramos-Goñi JM. An EQ-5D-5L value set based on Uruguayan population preferences. Qual Life Res. 2016 Feb;25(2):323-33. \doi{10.1007/s11136-015-1086-4}. PMID: 26242249.}
#' \item{\strong{USA}: Pickard AS, Law EH, Jiang R, Pullenayegum E, Shaw JW, Xie F, Oppe M, Boye KS, Chapman RH, Gong CL, Balch A, Busschbach JJV. United States Valuation of EQ-5D-5L Health States Using an International Protocol. Value in Health. 2019 Aug;22(8):931-941. \doi{10.1016/j.jval.2019.02.009}. PMID: 31426935.}
#' \item{\strong{Vietnam}: Mai VQ, Sun S, Minh HV, Luo N, Giang KB, Lindholm L, Sahlen KG. An EQ-5D-5L Value Set for Vietnam. Qual Life Res. 2020;29(7):1923-1933. \doi{10.1007/s11136-020-02469-7}. PMID: 32221805.}
#' \item{\strong{WePP}: Olsen JA, Lamu AN, Cairns J. In search of a common currency: A comparison of seven EQ-5D-5L value sets. Health Econ. 2018 Jan;27(1):39-49. \doi{10.1002/hec.3606}. Epub 2017 Oct 24. PMID: 29063633.}
#' }
#' @name VT
#' @export
"VT"

#' EQ-5D-Y-3L value set data
#'
#' EQ-5D-Y-3L value set calculation data for Belgium, Brazil, China, Germany, Hungary, 
#' Indonesia, Japan, Netherlands, Slovenia and Spain.
#'
#' @source
#'   \itemize{
#'     \item{\strong{Belgium}: Dewilde S, Roudijk B, Tollenaar NH, Ramos-Goñi JM. An EQ-5D-Y-3L Value Set for Belgium. Pharmacoeconomics. 2022 Nov 1:1–12. \doi{10.1007/s40273-022-01187-x}. Epub ahead of print.  PMID: 36316544.}
#'     \item{\strong{Brazil}: Espirito Santo CM, Miyamoto GC, Santos VS, Ben ÂJ, Finch AP, Roudijk B, de Jesus-Moraleida FR, Stein AT, Santos M, Yamato TP. Estimating an EQ-5D-Y-3L Value Set for Brazil. Pharmacoeconomics. 2024 Jul 2. \doi{10.1007/s40273-024-01404-9}. Epub ahead of print. PMID: 38954389.}
#'     \item{\strong{China}: Yang Z, Jiang J, Wang P, Jin X, Wu J, Fang Y, Feng D, Xi X, Li S, Jing M, Zheng B, Huang W, Luo N. Estimating an EQ-5D-Y-3L Value Set for China. Pharmacoeconomics. 2022 Nov 18. \doi{10.1007/s40273-022-01216-9}. Epub ahead of print. PMID: 36396878.}
#'     \item{\strong{Germany}: Kreimeier S, Mott D, Ludwig K, Greiner W; IMPACT HTA HRQoL Group. EQ-5D-Y Value Set for Germany. Pharmacoeconomics. 2022 May 23:1–13. \doi{10.1007/s40273-022-01143-9}. Epub ahead of print. PMID: 35604633.}
#'     \item{\strong{Hungary}: Rencz F, Ruzsa G, Bató A, Yang Z, Finch AP, Brodszky V. Value Set for the EQ-5D-Y-3L in Hungary. Pharmacoeconomics. 2022 Sep 20:1–11. \doi{10.1007/s40273-022-01190-2}. Epub ahead of print. PMID: 36123448.}
#'     \item{\strong{Indonesia}: Fitriana TS, Roudijk B, Purba FD, Busschbach JJV, Stolk E. Estimating an EQ-5D-Y-3L Value Set for Indonesia by Mapping the DCE onto TTO Values. Pharmacoeconomics. 2022 Nov 9. \doi{10.1007/s40273-022-01210-1}. Epub ahead of print.  PMID: 36348155.} 
#'     \item{\strong{Japan}: Shiroiwa T, Ikeda S, Noto S, Fukuda T, Stolk E. Valuation Survey of EQ-5D-Y Based on the International Common Protocol: Development of a Value Set in Japan. Med Decis Making. 2021 Mar 23:272989X211001859. \doi{10.1177/0272989X211001859}. Epub ahead of print. PMID: 33754886.}
#'     \item{\strong{Netherlands}:Roudijk B, Sajjad A, Essers B, Lipman S, Stalmeier P, Finch AP. A Value Set for the EQ-5D-Y-3L in the Netherlands. Pharmacoeconomics. 2022 Oct 10:1–11. \doi{10.1007/s40273-022-01192-0}. Epub ahead of print. PMID: 36216977.}
#'     \item{\strong{Slovenia}: Prevolnik Rupel V, Ogorevc M; IMPACT HTA HRQoL Group. EQ-5D-Y Value Set for Slovenia. Pharmacoeconomics. 2021 Feb 10. \doi{10.1007/s40273-020-00994-4}. Epub ahead of print. PMID: 33565048.}
#'     \item{\strong{Spain}: Ramos-Goñi JM, Oppe M, Estévez-Carrillo A, Rivero-Arias O; IMPACT HTA HRQoL Group. Accounting for Unobservable Preference Heterogeneity and Evaluating Alternative Anchoring Approaches to Estimate Country-Specific EQ-5D-Y Value Sets: A Case Study Using Spanish Preference Data. Value Health. 2022 May;25(5):835-843. \doi{10.1016/j.jval.2021.10.013}. Epub 2021 Dec 6. PMID: 35500952..}
#'   }
#'
#' @name Y3L
#' @export
"Y3L"
