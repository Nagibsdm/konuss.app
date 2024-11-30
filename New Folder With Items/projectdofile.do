tsset time

gen L_RealGDPmlnofeuro = log(RealGDPmlnofeuro)
gen L_ImpliedGDPdeflator201520 = log(ImpliedGDPdeflator201520)

gen D_RealGDPmlnofeuro = d.L_RealGDPmlnofeuro
gen D_ImpliedGDPdeflator201520 = d.L_ImpliedGDPdeflator201520
gen D_Tenyearbondyield = d.Tenyearbondyield

tsline L_ImpliedGDPdeflator201520
tsline L_RealGDPmlnofeuro
tsline Tenyearbondyield


dfgls Tenyearbondyield
dfgls L_ImpliedGDPdeflator201520 
dfgls L_RealGDPmlnofeuro 

tsline D_RealGDPmlnofeuro
tsline D_ImpliedGDPdeflator201520
tsline D_Tenyearbondyield

dfgls D_RealGDPmlnofeuro
dfgls D_ImpliedGDPdeflator201520
dfgls D_Tenyearbondyield


varsoc D_RealGDPmlnofeuro D_ImpliedGDPdeflator201520 D_Tenyearbondyield 


varbasic  D_ImpliedGDPdeflator201520 D_Tenyearbondyield D_RealGDPmlnofeuro , lags (1)


varstable , graph


varnorm

varlmar, mlag(1)

varwle



vargranger


irf graph oirf, impulse (D_Tenyearbondyield) response (D_ImpliedGDPdeflator201520)

irf graph oirf , impulse(D_Tenyearbondyield) response(D_RealGDPmlnofeuro)

irf graph oirf , impulse(D_Tenyearbondyield) response(D_Tenyearbondyield)

irf graph oirf , impulse(D_ImpliedGDPdeflator201520) response(D_ImpliedGDPdeflator201520)

irf graph oirf , impulse(D_ImpliedGDPdeflator201520) response (D_RealGDPmlnofeuro)

irf graph oirf , impulse(D_ImpliedGDPdeflator201520) response (D_Tenyearbondyield)

irf graph oirf , impulse(D_RealGDPmlnofeuro) response (D_ImpliedGDPdeflator201520)

irf graph oirf , impulse (D_RealGDPmlnofeuro) response(D_RealGDPmlnofeuro)

irf graph oirf , impulse (D_RealGDPmlnofeuro) response (D_Tenyearbondyield)