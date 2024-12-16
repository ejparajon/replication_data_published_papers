clear

cd "YOUR PATH" 

use "final_data_both_samples.dta"


*install estout 
*ssc install estout, replace

* Tables 1-3 see R file. 

* Tables 4 and 5
*does treatment affect perceptions of support? (manipulation check)

*ssi
reg ec_support i.en_scholars_treatments_2  if mturk==0
estadd local sample "SSI" , replace
eststo m1

reg ec_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if  mturk==0
estadd local sample "SSI" , replace
eststo m11

reg cs_support i.en_scholars_treatments_2  if (expert_type_2 == "climate science") & mturk==0
estadd local sample "SSI" , replace
eststo m2 

reg cs_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 == "climate science" ) & mturk==0
estadd local sample "SSI" , replace
eststo m21

reg ir_support i.en_scholars_treatments_2 if (expert_type_2 ==  "international relations") & mturk==0
estadd local sample "SSI" , replace
eststo m3

reg ir_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if (expert_type_2 ==  "international relations") & mturk==0
estadd local sample "SSI" , replace
eststo m31
 
reg econ_support i.en_scholars_treatments_2 if (expert_type_2 ==  "international economics") & mturk==0
estadd local sample "SSI" , replace
eststo m4

reg econ_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 ==  "international economics") & mturk==0
estadd local sample "SSI" , replace
eststo m41

*mturk
reg ec_support i.en_scholars_treatments_2  if  mturk==1
estadd local sample "mTurk" , replace
eststo m1mt 

reg ec_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk==1
estadd local sample "mTurk" , replace
eststo m11mt

reg cs_support i.en_scholars_treatments_2  if (expert_type_2 == "climate science") & mturk==1
estadd local sample "mTurk" , replace
eststo m2mt 

reg cs_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 == "climate science") & mturk==1
estadd local sample "mTurk" , replace
eststo m21mt

reg ir_support i.en_scholars_treatments_2 if (expert_type_2 ==  "international relations") & mturk==1
estadd local sample "mTurk" , replace
eststo m3mt

reg ir_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if (expert_type_2 ==  "international relations") & mturk==1
estadd local sample "mTurk" , replace
eststo m31mt
 
reg econ_support i.en_scholars_treatments_2 if (expert_type_2 ==  "international economics") & mturk==1
estadd local sample "mTurk" , replace
eststo m4mt

reg econ_support i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 ==  "international economics") & mturk==1
estadd local sample "mTurk" , replace
eststo m41mt

esttab m1 m2 m3 m4 m1mt m2mt m3mt m4mt using "Tables/table4.tex", compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) mgroups("Avg. Effect" "Climate Science" "Int'l Relations" "Int'l Econ." "Avg. Effect" "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3)  labels("Sample" "Obs." "\(R^2\)")) 

esttab m11 m21 m31 m41 m11mt m21mt m31mt m41mt using "Tables/table5.tex", compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) r2() mgroups("Avg. Effect" "Climate Science" "Int'l Relations" "Int'l Econ." "Avg. Effect" "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3)  labels("Sample" "Obs." "\(R^2\)")) 

estimates clear


* Tables 6 and 7 -- main results
reg cop21_dv i.en_scholars_treatments_2  if mturk==0 
estadd local sample "SSI" , replace
eststo m1 
reg cop21_dv i.en_scholars_treatments_2  if mturk==1
estadd local sample "mTurk" , replace
eststo m1mt 

reg cop21_dv i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk==0 
estadd local sample "SSI" , replace
eststo m11
reg cop21_dv i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk==1
estadd local sample "mTurk" , replace
eststo m11mt

reg cop21_dv i.en_scholars_treatments_2  if mturk == 0 & (expert_type_2 == "climate science")
estadd local sample "SSI" , replace
eststo m2 
reg cop21_dv i.en_scholars_treatments_2  if mturk == 1 & (expert_type_2 == "climate science")
estadd local sample "mTurk" , replace
eststo m2mt

reg cop21_dv i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk == 0 & (expert_type_2 == "climate science")
estadd local sample "SSI" , replace
eststo m21
reg cop21_dv i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk == 1 & (expert_type_2 == "climate science")
estadd local sample "mTurk" , replace
eststo m21mt

reg cop21_dv i.en_scholars_treatments_2 if mturk == 0 & (expert_type_2 == "international relations")
estadd local sample "SSI" , replace
eststo m3
reg cop21_dv i.en_scholars_treatments_2 if mturk == 1 & (expert_type_2 == "international relations")
estadd local sample "mTurk" , replace
eststo m3mt

reg cop21_dv i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if mturk == 0 & (expert_type_2 == "international relations")
estadd local sample "SSI" , replace
eststo m31
reg cop21_dv i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if mturk == 1 & (expert_type_2 == "international relations")
estadd local sample "mTurk" , replace
eststo m31mt
 
reg cop21_dv i.en_scholars_treatments_2 if mturk == 0 & (expert_type_2 == "international economics")
estadd local sample "SSI" , replace
eststo m4
reg cop21_dv i.en_scholars_treatments_2 if mturk == 1 & (expert_type_2 == "international economics")
estadd local sample "mTurk" , replace
eststo m4mt

reg cop21_dv i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk == 0 & (expert_type_2 == "international economics")
estadd local sample "SSI" , replace
eststo m41
reg cop21_dv i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk == 1 & (expert_type_2 == "international economics")
estadd local sample "mTurk" , replace
eststo m41mt


*Table 6
esttab m1 m2 m3 m4 m1mt m2mt m3mt m4mt using "Tables/table6.tex",  compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) mgroups("All" "Climate Science" "Int'l Relations" "Int'l Econ." "All" "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3) labels("Sample" "Obs." "\(R^2\)")) 

*Table 7
esttab m11 m21 m31 m41 m11mt m21mt m31mt m41mt using "Tables/table7.tex", compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) r2() mgroups("All" "Climate Science" "Int'l Relations" "Int'l Econ." "All" "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3)  labels("Sample" "Obs." "\(R^2\)")) 


estimates clear


* Tables 8 and 9 -- treatment effects by ideology. 
* See accompanying R file.



* Tables 10 and 11
*does treatment affect perceptions of knowledge? 
reg knowledge_cs i.en_scholars_treatments_2  if mturk == 0 & (expert_type_2 == "climate science")
estadd local sample "SSI" , replace
eststo m2 
reg knowledge_cs i.en_scholars_treatments_2  if mturk == 1 & (expert_type_2 == "climate science")
estadd local sample "mTurk" , replace
eststo m2mt


reg knowledge_cs i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age  ideology i.race quarter_income i.gender if mturk == 0 & (expert_type_2 == "climate science")
estadd local sample "SSI" , replace
eststo m21
reg knowledge_cs i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income i.gender if mturk == 1 & (expert_type_2 == "climate science")
estadd local sample "mTurk" , replace
eststo m21mt


reg knowledge_ir i.en_scholars_treatments_2 if mturk == 0 & (expert_type_2 == "international relations")
estadd local sample "SSI" , replace
eststo m3
reg knowledge_ir i.en_scholars_treatments_2 if mturk == 1 & (expert_type_2 == "international relations")
estadd local sample "mTurk" , replace
eststo m3mt


reg knowledge_ir i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if mturk == 0 & (expert_type_2 == "international relations")
estadd local sample "SSI" , replace
eststo m31
reg knowledge_ir i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if mturk == 1 & (expert_type_2 == "international relations")
estadd local sample "mTurk" , replace
eststo m31mt
 
reg knowledge_ie i.en_scholars_treatments_2 if mturk == 0 & (expert_type_2 == "international economics")
estadd local sample "SSI" , replace
eststo m4
reg knowledge_ie i.en_scholars_treatments_2 if mturk == 1 & (expert_type_2 == "international economics")
estadd local sample "mTurk" , replace
eststo m4mt

reg knowledge_ie i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk == 0 & (expert_type_2 == "international economics")
estadd local sample "SSI" , replace
eststo m41
reg knowledge_ie i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if mturk == 1 & (expert_type_2 == "international economics")
estadd local sample "mTurk" , replace
eststo m41mt



esttab m2 m3 m4 m2mt m3mt m4mt using "Tables/table10.tex", compress  substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) mgroups("Climate Science" "Int'l Relations" "Int'l Econ." "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3) labels("Sample" "Obs." "\(R^2\)")) 

esttab m21 m31 m41 m21mt m31mt m41mt using "Tables/table11.tex", compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) r2() mgroups("Climate Science" "Int'l Relations" "Int'l Econ." "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3) labels("Sample" "Obs." "\(R^2\)")) 


estimates clear



*does treatment affect perceptions of benefits to US?
reg agreement_fx i.en_scholars_treatments_2 if mturk==0
estadd local sample "SSI" , replace
eststo m1 

reg agreement_fx i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if mturk==0
estadd local sample "SSI" , replace
eststo m11

reg agreement_fx i.en_scholars_treatments_2  if (expert_type_2 == "climate science") & mturk==0
estadd local sample "SSI" , replace
eststo m2 

reg agreement_fx i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 == "climate science") & mturk==0
estadd local sample "SSI" , replace
eststo m21

reg agreement_fx i.en_scholars_treatments_2 if (expert_type_2 == "international relations") & mturk==0
estadd local sample "SSI" , replace
eststo m3

reg agreement_fx i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if (expert_type_2 == "international relations") & mturk==0
eststo m31

reg agreement_fx i.en_scholars_treatments_2 if (expert_type_2 == "international economics") & mturk==0
estadd local sample "SSI" , replace
eststo m4

reg agreement_fx i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 == "international economics") & mturk==0
estadd local sample "SSI" , replace
eststo m41

reg agreement_fx i.en_scholars_treatments_2 if mturk==1
estadd local sample "mTurk" , replace
eststo m1mt 

reg agreement_fx i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if mturk==1
estadd local sample "mTurk" , replace
eststo m11mt

reg agreement_fx i.en_scholars_treatments_2  if (expert_type_2 == "climate science") & mturk==1
estadd local sample "mTurk" , replace
eststo m2mt 

reg agreement_fx i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 == "climate science") & mturk==1
estadd local sample "mTurk" , replace
eststo m21mt

reg agreement_fx i.en_scholars_treatments_2 if (expert_type_2 == "international relations") & mturk==1
estadd local sample "mTurk" , replace
eststo m3mt

reg agreement_fx i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income  if (expert_type_2 == "international relations") & mturk==1
estadd local sample "mTurk" , replace
eststo m31mt

reg agreement_fx i.en_scholars_treatments_2 if (expert_type_2 == "international economics") & mturk==1
estadd local sample "mTurk" , replace
eststo m4mt

reg agreement_fx i.en_scholars_treatments_2 i.en_scholars_treatments_2 ib4.edu_new3 age i.gender ideology i.race quarter_income if (expert_type_2 == "international economics") & mturk==1
estadd local sample "mTurk" , replace
eststo m41mt

*Table 12
esttab m1 m2 m3 m4 m1mt m2mt m3mt m4mt using "Tables/table12.tex", compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) mgroups("All" "Climate Science" "Int'l Relations" "Int'l Econ." "All" "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3) labels("Sample" "Obs." "\(R^2\)")) 

*Table 13
esttab m11 m21 m31 m41 m11mt m21mt m31mt m41mt using "Tables/table13.tex", compress substitute( " 0.00" "ref." "(.)" " " ) label booktabs  b(2) star(+ 0.10 * 0.05 ** .01) r2() mgroups("All" "Climate Science" "Int'l Relations" "Int'l Econ." "All" "Climate Science" "Int'l Relations" "Int'l Econ.", pattern(1 1 1 1 1 1 1 1) prefix(\multicolumn{@span}{c}{) suffix(}) span erepeat(\cmidrule(lr){@span})) nonumbers nogaps se eqlabels(none) replace refcat(control "Treatment" 1.en_scholars_treatments_2 "Treatment" 1.gender "Gender" 19.race "Race/Ethnicity" 1.edu_new3 "Education", nolabel) stats(sample N r2, fmt(0 0 3) labels("Sample" "Obs." "\(R^2\)")) 
