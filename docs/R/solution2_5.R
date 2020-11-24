# ---------------------------
# Solution to Exercise 2.5
# November 24, 2017
# Bo Markussen
# ---------------------------

?power.prop.test
power.prop.test(power=0.8,p1=0.3,p2=0.25*0.3) # n=46
power.prop.test(power=0.8,p1=0.4,p2=0.25*0.4) # n=32
power.prop.test(power=0.8,p1=0.5,p2=0.25*0.5) # n=23
power.prop.test(power=0.9,p1=0.3,p2=0.25*0.3) # n=61
power.prop.test(power=0.9,p1=0.4,p2=0.25*0.4) # n=42
power.prop.test(power=0.9,p1=0.5,p2=0.25*0.5) # n=30

?power.t.test
power.t.test(n=19,sd=15,delta=8,type="one.sample") # power=0.59
power.t.test(n=19,sd=10,delta=8,type="one.sample") # power=0.91
