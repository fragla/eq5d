#' EQ-5D-5L Crosswalk data
#'
#' Crosswalk index value calculation table to calculate EQ-5D-3L indices from EQ-5D-5L data
#'   for Denmark, France, Germany, Japan, Netherlands, Russia, Spain, Thailand, UK, USA 
#'   and Zimbabwe.
#'      
#' @source
#' van Hout B, Janssen MF, et al. Interim scoring for the EQ-5D-5L: Mapping the EQ-5D-5L 
#'   to EQ-5D-3L value sets. Value in Health 2012 Jul-Aug;15(5):708-15. doi: 10.1016/j.jval.2012.02.008. 
#'   \href{https://pubmed.ncbi.nlm.nih.gov/22867780/}{PubMed}
#'   
#' Omelyanovskiy V, Musina N, Ratushnyak S, Bezdenezhnykh T, Fediaeva V, Roudijk B, 
#' Purba FD. Valuation of the EQ-5D-3L in Russia. Qual Life Res. 2021 Mar 13. 
#' doi: 10.1007/s11136-021-02804-6. Epub ahead of print. 
#' \href{https://pubmed.ncbi.nlm.nih.gov/33713323/}{PubMed}.
#'
#' \href{https://euroqol.org/wp-content/uploads/2018/02/EQ-5D-5L_Crosswalk_Index_Value_Calculator_v2.xls}{EQ-5D-5L Crosswalk Index Value Calculator}
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
#' Hernández Alava M, Pudney S, Wailoo A. Estimating the Relationship Between EQ-5D-5L and EQ-5D-3L: Results From an English Population Study [EEPRU Report]. University of Sheffield & University of York. \href{https://eepru.sites.sheffield.ac.uk/projects/estimating-the-relationship-between-eq-5d-5l-and-eq-5d-3l}{Article}
#' 
#' Hernández-Alava M, Pudney S. Econometric modelling of multiple self-reports of health states: The switch from EQ-5D-3L to EQ-5D-5L in evaluating drug therapies for rheumatoid arthritis. J Health Econ. 2017 Sep;55:139-152. doi: 10.1016/j.jhealeco.2017.06.013. Epub 2017 Jul 4. \href{https://pubmed.ncbi.nlm.nih.gov/28778350}{PubMed}.
#' 
#' \href{https://nicedsu.sites.sheffield.ac.uk/methods-development/mapping-eq-5d-5l-to-3l}{NICE DSU mapping website}.
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
#' Hernández Alava M, Pudney S, Wailoo A. Estimating the Relationship Between EQ-5D-5L and EQ-5D-3L: Results From an English Population Study [EEPRU Report]. University of Sheffield & University of York. \href{https://eepru.sites.sheffield.ac.uk/projects/estimating-the-relationship-between-eq-5d-5l-and-eq-5d-3l}{Article}
#' 
#' Hernández-Alava M, Pudney S. Econometric modelling of multiple self-reports of health states: The switch from EQ-5D-3L to EQ-5D-5L in evaluating drug therapies for rheumatoid arthritis. J Health Econ. 2017 Sep;55:139-152. doi: 10.1016/j.jhealeco.2017.06.013. Epub 2017 Jul 4. \href{https://pubmed.ncbi.nlm.nih.gov/28778350}{PubMed}.
#' 
#' \href{https://nicedsu.sites.sheffield.ac.uk/methods-development/mapping-eq-5d-5l-to-3l}{NICE DSU mapping website}.
#' 
#' @name DSU5L
#' @export
"DSU5L"

#' EQ-5D-3L Reverse Crosswalk data
#'
#' Reverse Crosswalk index value table to calculate EQ-5D-5L indices from EQ-5D-3L data
#'   for England, Germany, Netherlands and USA. Table uses the values of van Hout et al
#'   from the EuroQol analysis tools webpage.
#'      
#' @source
#' \href{https://euroqol.org/support/analysis-tools/}{Reverse crosswalk datasets}
#' 
#' @name RCW
#' @export       
"RCW"

#' EQ-5D-3L TTO value set data
#'
#' Coefficients for the estimation of the EQ-5D-3L index values based on TTO valuation studies
#'   for Argentina, Australia, Brazil, Canada, Chile, China, Denmark, Ecuador, France, 
#'   Germany, Hungary, Italy, Japan, Netherlands, Poland, Portugal, Russia, Singapore, 
#'   South Korea, Spain, Sri Lanka, Sweden, Taiwan, Thailand, Trinidad and Tobago, Tunisia,
#'   UK, USA and Zimbabwe.
#'      
#' @source
#' Szende, A., Oppe, M., & de Charro, F. (2007), Comparative review of Time Trade-Off value sets. 
#'   In Szende, A., Oppe, M., & Devlin, N. (Ed.), EQ-5D Value Sets: Inventory, Comparative Review
#'   and User Guide (pp. 27-28). Dordrecht, The Netherlands: Springer.
#'   
#' Janssen, B., Szende, A., & Ramos-Goñi JM. (2014), Data and Methods.
#'   Szende, A., Janssen, B., & Cabasés, J. (Ed.), In Self-Reported Population Health: An 
#'   International Perspective based on EQ-5D (p 13). Dordrecht, The Netherlands: Springer.
#' 
#' \strong{Australia}: Viney R, Norman R, King MT, Cronin P, Street DJ, Knox S, Ratcliffe J. Time trade-off derived EQ-5D weights for Australia. Value Health. 2011 Sep-Oct;14(6):928-36. doi: 10.1016/j.jval.2011.04.009. \href{https://pubmed.ncbi.nlm.nih.gov/21914515/}{PubMed}
#' 
#' \strong{Brazil}: Viegas Andrade M, Noronha K, Kind P, Maia AC, Miranda de Menezes R, De Barros Reis C, Nepomuceno Souza M, Martins D, Gomes L, Nichele D, Calazans J, Mascarenhas T, Carvalho L, Lins C. Societal Preferences for EQ-5D Health States from a Brazilian Population Survey. Value in Health Regional Issues 2013;2(3):405–412. \href{https://pubmed.ncbi.nlm.nih.gov/29702778/}{PubMed}  
#' 
#' \strong{Canada}: Bansback N, Tsuchiya A, Brazier J, Anis A. Canadian valuation of EQ-5D health states: preliminary value set and considerations for future valuation studies. PLoS One. 2012;7(2):e31115. \href{https://pubmed.ncbi.nlm.nih.gov/22328929/}{PubMed}
#' 
#' \strong{Chile}: Zarate V, Kind P, Valenzuela P, Vignau A, Olivares-Tirado P, Munoz A. Social valuation of EQ-5D health states: the Chilean case. Value in Health. 2011 Dec;14(8):1135-41. \href{https://pubmed.ncbi.nlm.nih.gov/22152184/}{PubMed}
#' 
#' \strong{China}: Liu GG, Wu H, Li M, Gao C, Luo N. Chinese time trade-off values for EQ-5D health states. Value Health. 2014 Jul;17(5):597-604. doi: 10.1016/j.jval.2014.05.007. Epub 2014 Jul 23. \href{https://pubmed.ncbi.nlm.nih.gov/25128053/}{PubMed}
#'
#' \strong{Ecuador}: Lucio R, Flores V,  Granja M, Mata G. Resultados de la encuesta de valoración social de los estados de salud de lAños de vida ajustados por calidad (QALY'S). 2019. \href{https://www.salud.gob.ec/wp-content/uploads/2019/08/resultados_encuesta_valoracion_estados_salud.pdf}{Link}
#'
#' \strong{Hungary}: Rencz F, Brodszky V, Gulácsi L, Golicki D, Ruzsa G, Pickard AS, Law EH, Péntek M. Parallel Valuation of the EQ-5D-3L and EQ-5D-5L by Time Trade-Off in Hungary. Value Health. 2020 Sep;23(9):1235-1245. doi: 10.1016/j.jval.2020.03.019. Epub 2020 Aug 12. \href{https://pubmed.ncbi.nlm.nih.gov/32940242/}{PubMed}
#'   
#' \strong{Poland}: Golicki D, Jakubczyk M, Niewada M, Wrona W, Busschbach JJ. Valuation of EQ-5D health states in Poland: first TTO-based social value set in Central and Eastern Europe. Value in Health. 2010;13(2):289-97. \href{https://pubmed.ncbi.nlm.nih.gov/19744296/}{PubMed}
#' 
#' \strong{Portugal}: Ferreira LN, Ferreira PL, Pereira LN, Oppe M. The valuation of the EQ-5D in Portugal. Qual Life Res. 2014 Mar;23(2):413-23. doi: 10.1007/s11136-013-0448-z. Epub 2013 Jun 8. \href{https://pubmed.ncbi.nlm.nih.gov/23748906/}{PubMed}
#' 
#' \strong{Russia}: Omelyanovskiy V, Musina N, Ratushnyak S, Bezdenezhnykh T, Fediaeva V, Roudijk B, Purba FD. Valuation of the EQ-5D-3L in Russia. Qual Life Res. 2021 Mar 13. doi: 10.1007/s11136-021-02804-6. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/33713323/}{PubMed}
#' 
#' \strong{Singapore}: Luo N, Wang P, Thumboo J, Lim YW, Vrijhoef HJ. Valuation of EQ-5D-3L health states in Singapore: modeling of time trade-off values for 80 empirically observed health states. Pharmacoeconomics. 2014 May;32(5):495-507. doi: 10.1007/s40273-014-0142-1. \href{https://pubmed.ncbi.nlm.nih.gov/24519603/}{PubMed}
#' 
#' \strong{Sri Lanka}: Kularatna S, Whitty JA, Johnson NW, Jayasinghe R, Scuffham PA. Valuing EQ-5D health states for Sri Lanka. Qual Life Res. 2015 Jul;24(7):1785-93. doi:10.1007/s11136-014-0906-2. Epub 2014 Dec 28. PubMed PMID: \href{https://pubmed.ncbi.nlm.nih.gov/25543271/}{PubMed}
#' 
#' \strong{Sweden}: Burström K, Sun S, Gerdtham UG, Henriksson M, Johannesson M, Levin LÅ, Zethraeus N. Swedish experience-based value sets for EQ-5D health states. Qual Life Res. 2014 Mar;23(2):431-42. doi: 10.1007/s11136-013-0496-4. \href{https://pubmed.ncbi.nlm.nih.gov/23975375/}{PubMed}
#' 
#' \strong{Taiwan}: Lee HY, Hung MC, Hu FC, Chang YY, Hsieh CL, Wang JD. Estimating quality weights for EQ-5D (EuroQol-5 dimensions) health states with the time trade-off method in Taiwan. J Formos Med Assoc. 2013;112(11):699-706. \href{https://pubmed.ncbi.nlm.nih.gov/24183199/}{PubMed}
#' 
#' \strong{Thailand}: Tongsiri S, Cairns J. Estimating population-based values for EQ-5D health states in Thailand. Value Health. 2011 Dec;14(8):1142-5. doi: 10.1016/j.jval.2011.06.005. \href{https://pubmed.ncbi.nlm.nih.gov/22152185/}{PubMed}
#' 
#' \strong{Trinidad and Tobago}: Bailey H, Stolk E, Kind P. Toward Explicit Prioritization for the Caribbean: An EQ-5D Value Set for Trinidad and Tobago. Value Health Reg Issues. 2016 Dec;11:60-67. doi: 10.1016/j.vhri.2016.07.010. \href{https://pubmed.ncbi.nlm.nih.gov/27986200/}{PubMed}
#' 
#' \strong{Tunisia}: Chemli J, Drira C, Felfel H, Roudijk B, Al Sayah F, Kouki M, Kooli A, Razgallah Khrouf M. Valuing health-related quality of life using a hybrid approach: Tunisian value set for the EQ-5D-3L. Qual Life Res. 2021 Jan 14. doi: 10.1007/s11136-020-02730-z. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/33447958/}{PubMed}
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
#' \strong{Iran}: Goudarzi R, Zeraati H, Akbari Sari A, Rashidian A, Mohammad K. Population-Based Preference Weights for the EQ-5D Health States Using the Visual Analogue Scale (VAS) in Iran. Iran Red Crescent Med J. 2016 Feb 13;18(2):e21584. doi: 10.5812/ircmj.21584. \href{https://pubmed.ncbi.nlm.nih.gov/27186384/}{PubMed}
#' 
#' \strong{Malaysia}: Yusof FA, Goh A, Azmi S. Estimating an EQ-5D value set for Malaysia using time trade-off and visual analogue scale methods. Value Health. 2012 Jan-Feb;15(1 Suppl):S85-90. doi: 10.1016/j.jval.2011.11.024. \href{https://pubmed.ncbi.nlm.nih.gov/22265073/}{PubMed}
#' 
#' @name VAS
#' @export
"VAS"

#' EQ-5D-5L VT value set data
#'
#' EQ-5D-5L VT value set calculation data for Belgium, Canada, China, Denmark, 
#'   Egypt, England, Ethiopia, France, Germany, Hong Kong, Hungary, India, 
#'   Indonesia, Ireland, Italy, Japan, Malaysia, Mexico, Netherlands, 
#'   New Zealand, Peru, Poland, Portugal, South Korea, Spain, Sweden, Taiwan, 
#'   Thailand, Uganda, Uruguay, USA, Vietnam and Western Preference Pattern (WePP).
#'
#' @source 
#'   \enumerate{
#'     \item{\strong{Belgium}: Bouckaert N, Gerkens S, Devriese S, Cleemput I. An EQ-5D-5L value set for Belgium – How to value health- related quality of life? Health Services Research (HSR) Brussels: Belgian Health Care Knowledge Centre (KCE). 2021. KCE Reports 342. D/2021/10.273/19. \href{https://www.kce.fgov.be/sites/default/files/2021-11/KCE_342_EQ-5D-5L_value_set_for_Belgium_Report_1.pdf}{PDF}}
#'     \item{\strong{Canada}: Xie F, Pullenayegum E, Gaebel K, Bansback N, Bryan S, Ohinmaa A, Poissant L, Johnson JA. A Time Trade-off-derived Value Set of the EQ-5D-5L for Canada. Med Care. 2016;54(1):98-105. \href{https://pubmed.ncbi.nlm.nih.gov/26492214/}{PubMed}}
#'     \item{\strong{China}: Luo N, Liu G, Li M, Guan H, Jin X, Rand-Hendriksen K. Estimating an EQ-5D-5L Value Set for China. Value in Health. 2017 Apr;20(4):662-669. doi: 10.1016/j.jval.2016.11.016. Epub 2017 Feb 9. \href{https://pubmed.ncbi.nlm.nih.gov/28408009/}{PubMed}}
#'     \item{\strong{Denmark}: Jensen CE, Sørensen SS, Gudex C, Jensen MB, Pedersen KM, Ehlers LH. The Danish EQ-5D-5L Value Set: A Hybrid Model Using cTTO and DCE Data. Appl Health Econ Health Policy. 2021 Feb 2. doi: 10.1007/s40258-021-00639-3. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/33527304/}{PubMed}}
#'     \item{\strong{England}: Devlin N, Shah K, Feng Y, Mulhern B, van Hout B. Valuing health-related quality of Life: An EQ-5D-5L Value Set for England. Health Economics. 2018 Jan;27(1):1-22 \href{https://pubmed.ncbi.nlm.nih.gov/28833869/}{PubMed}}
#'     \item{\strong{Egypt}: Al Shabasy S, Abbassi M, Finch A, Roudijk B, Baines D, Farid S. The EQ-5D-5L Valuation Study in Egypt. Pharmacoeconomics. 2021 Nov 17:1–15. doi: 10.1007/s40273-021-01100-y. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/34786590/}{PubMed}}
#'     \item{\strong{Ethiopia}: Welie AG, Gebretekle GB, Stolk E, Mukuria C, Krahn MD, Enquoselassie F, Fenta TG. Valuing Health State: An EQ-5D-5L Value Set for Ethiopians. Value Health Reg Issues. 2019 Nov 1;22:7-14. doi: 10.1016/j.vhri.2019.08.475. \href{https://pubmed.ncbi.nlm.nih.gov/31683254/}{PubMed}}
#'     \item{\strong{France}: Andrade LF, Ludwig K, Goni JMR, Oppe M, de Pouvourville G. A French Value Set for the EQ-5D-5L. Pharmacoeconomics. 2020 Jan 8. doi: 10.1007/s40273-019-00876-4. \href{https://pubmed.ncbi.nlm.nih.gov/31912325/}{PubMed}}
#'     \item{\strong{Germany}: Ludwig K, Graf von der Schulenburg JM, Greiner W. German Value Set for the EQ-5D-5L. Pharmacoeconomics. 2018 Feb;36(6):663-674. doi: 10.1007/s40273-018-0615-8. \href{https://pubmed.ncbi.nlm.nih.gov/29460066/}{PubMed}}
#'     \item{\strong{HongKong}: Wong ELY, Ramos-Goñi JM, Cheung AWL, Wong AYK, Rivero-Arias O. Assessing the Use of a Feedback Module to Model EQ-5D-5L Health States Values in Hong Kong. Patient. 2018 Apr;11(2):235-247. doi: 10.1007/s40271-017-0278-0. \href{https://pubmed.ncbi.nlm.nih.gov/29019161/}{PubMed}}
#'     \item{\strong{Hungary}: Rencz F, Brodszky V, Gulácsi L, Golicki D, Ruzsa G, Pickard AS, Law EH, Péntek M. Parallel Valuation of the EQ-5D-3L and EQ-5D-5L by Time Trade-Off in Hungary. Value Health. 2020 Sep;23(9):1235-1245. doi: 10.1016/j.jval.2020.03.019. Epub 2020 Aug 12. \href{https://pubmed.ncbi.nlm.nih.gov/32940242/}{PubMed}}
#'     \item{\strong{India}: Jyani G, Sharma A, Prinja S, Kar SS, Trivedi M, Patro BK, Goyal A, Purba FD, Finch AP, Rajsekar K, Raman S, Stolk E, Kaur M. Development of an EQ-5D Value Set for India Using an Extended Design (DEVINE) Study: The Indian 5-Level Version EQ-5D Value Set. Value Health. 2022 Jul;25(7):1218-1226. doi: 10.1016/j.jval.2021.11.1370. Epub 2022 Jan 5. \href{https://pubmed.ncbi.nlm.nih.gov/35779943/}{PubMed}}
#'     \item{\strong{Indonesia}: Purba FD, Hunfeld JAM, Iskandarsyah A, Fitriana TS, Sadarjoen SS, Ramos-Goñi JM, Passchier J, Busschbach JJ. The Indonesian EQ-5D-5L Value Set. PharmacoEconomics. 2017 Nov;35(11)1153-1165. doi: 10.1007/s40273-017-0538-9. \href{https://pubmed.ncbi.nlm.nih.gov/28695543/}{PubMed}}
#'     \item{\strong{Ireland}: Hobbins A, Barry L, Kelleher D, Shah K, Devlin N, Ramos Goñi JM, O’Neill C. Utility Values for Health States in Ireland: A Value Set for the EQ-5D-5L. PharmacoEconomics. 2018 Nov;36(11):1345-1353. doi: 10.1007/s40273-018-0690-x. \href{https://pubmed.ncbi.nlm.nih.gov/30051267/}{PubMed}}
#'     \item{\strong{Italy}: Finch AP, Meregaglia M, Ciani O, Roudijk B, Jommi C. An EQ-5D-5L value set for Italy using videoconferencing interviews and feasibility of a new mode of administration. Social Science & Medicine. 2021 Oct. doi: 10.1016/j.socscimed.2021.114519. \href{https://www.sciencedirect.com/science/article/pii/S0277953621008510}{Journal}}
#'     \item{\strong{Japan}: Shiroiwa T, Ikeda S, Noto S, Igarashi A, Fukuda T, Saito S, Shimozuma K. Comparison of Value Set Based on DCE and/or TTO Data: Scoring for EQ-5D-5L Health States in Japan. Value in Health. 2016 Jul-Aug;19(5):648-54. \href{https://pubmed.ncbi.nlm.nih.gov/27565282/}{PubMed}}
#'     \item{\strong{Malaysia}: Shafie AA; Vasan Thakumar A; Lim CJ;Luo N; Rand-Hendriksen K; Yusof FA. EQ-5D-5L Valuation for the Malaysian Population. PharmacoEconomics. 2019 May;37(5):715-725. doi: 10.1007/s40273-018-0758-7. \href{https://pubmed.ncbi.nlm.nih.gov/30535779/}{PubMed}}
#'     \item{\strong{Mexico}: Gutierrez-Delgado C, Galindo-Suárez RM, Cruz-Santiago C, Shah K, Papadimitropoulos M, Feng Y, Zamora B, Devlin N. EQ-5D-5L Health-State Values for the Mexican Population. Appl Health Econ Health Policy. 2021 Nov;19(6):905-914. doi: 10.1007/s40258-021-00658-0. Epub 2021 Jun 26. \href{https://pubmed.ncbi.nlm.nih.gov/34173957/}{PubMed}}
#'     \item{\strong{Netherlands}: Versteegh MM, Vermeulen KM, Evers SM, de Wit GA, Prenger R, Stolk EA. Dutch Tariff for the Five-Level Version of EQ-5D. Value in Health. 2016 Jun;19(4):343-52.  doi: 10.1016/j.jval.2016.01.003. \href{https://pubmed.ncbi.nlm.nih.gov/27325326/}{PubMed}}
#'     \item{\strong{New Zealand}: Sullivan T, Hansen P, Ombler F, Derrett S, Devlin N. A new tool for creating personal and social EQ-5D-5L value sets, including valuing 'dead'. Soc Sci Med. 2020 Feb;246:112707. doi: 10.1016/j.socscimed.2019.112707. Epub 2019 Nov 30. \href{https://pubmed.ncbi.nlm.nih.gov/31945596/}{PubMed}}
#'     \item{\strong{Peru} Augustovski F, Belizán M, Gibbons L, Reyes N, Stolk E, Craig BM, Tejada RA. Peruvian Valuation of the EQ-5D-5L: A Direct Comparison of Time Trade-Off and Discrete Choice Experiments. Value Health. 2020;23(7):880-888. doi:10.1016/j.jval.2020.05.004. \href{https://pubmed.ncbi.nlm.nih.gov/32762989/}{PubMed}}
#'     \item{\strong{Philippines} Miguel RTD, Rivera AS, Cheng KJG, Rand K, Purba FD, Luo N, Zarsuelo MA, Genuino-Marfori AJ, Florentino-Fariñas I, Guerrero AM, Lam HY. Estimating the EQ-5D-5L value set for the Philippines. Qual Life Res. 2022 May 9. doi: 10.1007/s11136-022-03143-w. \href{https://pubmed.ncbi.nlm.nih.gov/35532835/}{PubMed}}
#'     \item{\strong{Poland} Golicki D, Jakubczyk M, Niewada M, Wrona W, Busschbach JJ. Valuation of EQ-5D health states in Poland: first TTO-based social value set in Central and Eastern Europe. Value Health. 2010 Mar-Apr;13(2):289-97. doi: 10.1111/j.1524-4733.2009.00596.x. \href{https://pubmed.ncbi.nlm.nih.gov/19744296/}{PubMed}}
#'     \item{\strong{Portugal} Ferreira PL, Antunes P, Ferreira LN, Pereira LN, Ramos-Goñi JM. A hybrid modelling approach for eliciting health state preferences: the Portuguese EQ-5D-5L value set. Qual Life Res. 2019 Jun 14. doi: 10.1007/s11136-019-02226-5. \href{https://pubmed.ncbi.nlm.nih.gov/31201730/}{PubMed}}
#'     \item{\strong{Romania} Olariu E, Mohammed W, Oluboyede Y, Caplescu R, Niculescu-Aron IG, Paveliu MS, Vale L. EQ-5D-5L: a value set for Romania. Eur J Health Econ. 2022 Jun 10. doi: 10.1007/s10198-022-01481-7. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/35688994/}{PubMed}}
#'     \item{\strong{South Korea} Kim SH, Ahn J, Ock M, Shin S, Park J, Luo N, Jo MW. The EQ-5D-5L valuation study in Korea. Qual Life Res. 2016 Jul;25(7):1845-52. doi: 10.1007/s11136-015-1205-2. \href{https://pubmed.ncbi.nlm.nih.gov/26961008/}{PubMed}}
#'     \item{\strong{Spain}: Ramos-Goñi JM, Craig B, Oppe M, Ramallo-Fariña Y, Pinto-Prades JL, Luo N, Rivero-Arias O. Handling data quality issues to estimate the Spanish EQ-5D-5L Value Set using a hybrid interval regression approach.  Value in Health 2018 May;21(5):596-604. doi: 10.1016/j.jval.2017.10.023. \href{https://pubmed.ncbi.nlm.nih.gov/29753358/}{PubMed}}
#'     \item{\strong{Sweden}: Burström K, Teni FS, Gerdtham UG, Leidl R, Helgesson G, Rolfson O, Henriksson M. Experience-Based Swedish TTO and VAS Value Sets for EQ-5D-5L Health States. Pharmacoeconomics. 2020 Apr 20. doi: 10.1007/s40273-020-00905-7. \href{https://pubmed.ncbi.nlm.nih.gov/32307663/}{PubMed}}
#'     \item{\strong{Taiwan}: Lin HW, Li CI, Lin FJ, Chang JY, Gau CS, Luo N, Pickard AS, Ramos Goñi JM, Tang CH, Hsu CN. Valuation of the EQ-5D-5L in Taiwan. PLoS One. 2018; 13(12):: e0209344. doi: 10.1371/journal.pone.0209344. \href{https://www.ncbi.nlm.nih.gov/pmc/articles/PMC6306233/}{PubMed}}
#'     \item{\strong{Thailand} Pattanaphesaj J, Thavorncharoensap M, Ramos-Goñi JM, Tongsiri S, Ingsrisawang L, Teerawattananon Y. The EQ-5D-5L Valuation study in Thailand. Expert Review of Pharmacoeconomics & Outcomes Research. 2018 Oct;18(5):551-558. doi: 10.1080/14737167.2018.1494574 \href{https://pubmed.ncbi.nlm.nih.gov/29958008/}{PubMed}}
#'     \item{\strong{Uganda} Yang F, Katumba KR, Roudijk B, Yang Z, Revill P, Griffin S, Ochanda PN, Lamorde M, Greco G, Seeley J, Sculpher M. Developing the EQ-5D-5L Value Set for Uganda Using the 'Lite' Protocol. Pharmacoeconomics. 2021 Nov 29:1–13. doi: 10.1007/s40273-021-01101-x. \href{https://pubmed.ncbi.nlm.nih.gov/34841471/}{PubMed}}
#'     \item{\strong{Uruguay}: Augustovski F, Rey-Ares L, Irazola V, Garay OU, Gianneo O, Fernández G, Morales M, Gibbons L, Ramos-Goñi JM. An EQ-5D-5L value set based on Uruguayan population preferences. Qual Life Res. 2016 Feb;25(2):323-33. doi: 10.1007/s11136-015-1086-4. \href{https://pubmed.ncbi.nlm.nih.gov/26242249/}{PubMed}}
#'     \item{\strong{USA}: Pickard AS, Law EH, Jiang R, Pullenayegum E, Shaw JW, Xie F, Oppe M, Boye KS, Chapman RH, Gong CL, Balch A, Busschbach JJV. United States Valuation of EQ-5D-5L Health States Using an International Protocol. Value in Health. 2019 Aug;22(8):931-941. doi: 10.1016/j.jval.2019.02.009. \href{https://pubmed.ncbi.nlm.nih.gov/31426935/}{PubMed}}
#'     \item{\strong{Vietnam}: Mai VQ, Sun S, Minh HV, Luo N, Giang KB, Lindholm L, Sahlen KG. An EQ-5D-5L Value Set for Vietnam. Qual Life Res. 2020;29(7):1923-1933. doi:10.1007/s11136-020-02469-7. \href{https://pubmed.ncbi.nlm.nih.gov/32221805/}{PubMed}}
#'     \item{\strong{WePP}: Olsen JA, Lamu AN, Cairns J. In search of a common currency: A comparison of seven EQ-5D-5L value sets. Health Econ. 2018 Jan;27(1):39-49. doi: 10.1002/hec.3606. Epub 2017 Oct 24. \href{https://pubmed.ncbi.nlm.nih.gov/29063633/}{PubMed}}.
#'   }
#'   
#' @name VT
#' @export
"VT"

#' EQ-5D-Y value set data
#'
#' EQ-5D-Y value set calculation data for Japan and Slovenia.
#'
#' @source 
#'   \enumerate{
#'     \item{\strong{Germany}: Kreimeier S, Mott D, Ludwig K, Greiner W; IMPACT HTA HRQoL Group. EQ-5D-Y Value Set for Germany. Pharmacoeconomics. 2022 May 23:1–13. doi: 10.1007/s40273-022-01143-9. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/35604633/}{PubMed}}
#'     \item{\strong{Japan}: Shiroiwa T, Ikeda S, Noto S, Fukuda T, Stolk E. Valuation Survey of EQ-5D-Y Based on the International Common Protocol: Development of a Value Set in Japan. Med Decis Making. 2021 Mar 23:272989X211001859. doi: 10.1177/0272989X211001859. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/33754886/}{PubMed}}
#'     \item{\strong{Slovenia}: Prevolnik Rupel V, Ogorevc M; IMPACT HTA HRQoL Group. EQ-5D-Y Value Set for Slovenia. Pharmacoeconomics. 2021 Feb 10. doi: 10.1007/s40273-020-00994-4. Epub ahead of print. \href{https://pubmed.ncbi.nlm.nih.gov/33565048/}{PubMed}}
#'     \item{\strong{Spain}: Ramos-Goñi JM, Oppe M, Estévez-Carrillo A, Rivero-Arias O; IMPACT HTA HRQoL Group. Accounting for Unobservable Preference Heterogeneity and Evaluating Alternative Anchoring Approaches to Estimate Country-Specific EQ-5D-Y Value Sets: A Case Study Using Spanish Preference Data. Value in Health. 2021 Dec 04. doi: 10.1016/j.jval.2021.10.013.}
#'   }
#'   
#' @name Y
#' @export
"Y"
