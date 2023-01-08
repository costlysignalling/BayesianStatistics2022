library(rethinking)

data<-c("heads","tails","heads","heads")
data

#fair coin
sample(c("heads","tails"),size=4,prob=c(0.5,0.5),replace=T)
#unfair coin
sample(c("heads","tails"),size=4,prob=c(0.8,0.2),replace=T)

d.fair<-replicate(n=1000,sample(c("heads","tails"),size=4,prob=c(0.5,0.5),replace=T))
str(d.fair)

#I do not like this in "wide" format
d.fair<-t(d.fair)

head(d.fair)

d.tric<-t(replicate(n=1000,sample(c("heads","tails"),size=4,prob=c(0.8,0.2),replace=T)))
head(d.tric)

#So ok. How many cointoss sequences with each coin are exactly like mine
pasted.data<-paste(data,collapse=" ")

pasted.fair<-apply(d.fair,1,paste,collapse=" ")
pasted.tric<-apply(d.tric,1,paste,collapse=" ")

pasted.tric

#How many are identical in each simulated set?
pasted.data==pasted.fair

#In total (remember, TRUE is 1, FALSE is 0)
sum(pasted.data==pasted.fair)
sum(pasted.data==pasted.tric)

#So what is the resulting probability?
#From two hypotheses that I considered I got
match.fair<-sum(pasted.data==pasted.fair)
match.tric<-sum(pasted.data==pasted.tric)
match.fair+match.tric #cases altogether

match.fair
match.tric

(simul.p1<-match.fair/(match.fair+match.tric))
(simul.p2<-match.tric/(match.fair+match.tric))

data<-data=="heads"
data

plot(NULL,xlim=c(-2,2),ylim=c(0,2),bty="n",axes=F,xlab="",ylab="")
points(0,0,cex=2,lwd=2,pch=21,bg=0)
segments(c(0,0),c(0,0),c(-1,1),c(1,1),lwd=c(4,1))
points(c(-1,1),c(1,1),cex=2,lwd=2,pch=21,bg=0)
segments(c(-1,-1),c(1,1),-1+c(-1,1)*0.8,1+c(1,1)*0.8,lwd=c(4,1))
segments(c(1,1),c(1,1),1+c(-1,1)*0.8,1+c(1,1)*0.8,lwd=c(4,1))
points(-1+c(-1,1)*0.8,1+c(1,1)*0.8,cex=2,lwd=2,pch=21,bg=0)
points(1+c(-1,1)*0.8,1+c(1,1)*0.8,cex=2,lwd=2,pch=21,bg=0)
text(c(-1+c(-1,1)*0.8,1+c(-1,1)*0.8),c(1+c(1,1)*0.8,1+c(1,1)*0.8),
     labels=c("0.8*0.8=0.64",
            "0.8*0.2=0.16",
            "0.2*0.8=0.16",
            "0.2*0.2=0.04"),pos=c(3,3,4,3))

#It is easy, right?
#So the probability of TRUE FALSE TRUE TRUE with coin characterized by a parameter of heads probability p=0.8 is
(calcul.l2<-0.8*0.2*0.8*0.8)

#And the probability of the same data with the fair coin is
(calcul.l1<-0.5*0.5*0.5*0.5)

(calcul.p1<-calcul.l1/(calcul.l1+calcul.l2))
(calcul.p2<-calcul.l2/(calcul.l1+calcul.l2))

simul.p1
simul.p2

# P(H|d) = P(d|H)/P(d)

sum(data)

#So we got to a more abstract level

choose(5,2)

choose(2,2) #2 heads out of two
choose(2,0) #0 heads, all tails
choose(2,1)

plot(NULL,xlim=c(-2,2),ylim=c(0,2),bty="n",axes=F,xlab="",ylab="")
points(0,0,cex=2,lwd=2,pch=21,bg=0)
segments(c(0,0),c(0,0),c(-1,1),c(1,1),lwd=c(4,1))
points(c(-1,1),c(1,1),cex=2,lwd=2,pch=21,bg=0)
segments(c(-1,-1),c(1,1),-1+c(-1,1),1+c(1,1),lwd=c(4,1))
segments(c(1,1),c(1,1),1+c(-1,1),1+c(1,1),lwd=c(4,1))
points(c(-2,0,2),c(2,2,2),cex=2,lwd=2,pch=21,bg=0)
text(c(-2,0,2),c(2,2,2),
     labels=c("1*0.8*0.8=0.64",
              "2*0.8*0.2=0.32",
              "1*0.2*0.2=0.04"),pos=c(3,3,3),xpd=NA)

dbinom(x=1,size=2,prob=0.8)

dbinom(1,2,0.8)

#This is quite clearly the probability of ending in the middle.

dbinom(x=1,size=2,prob=0.8)
(binom.l1<-dbinom(3,4,0.5)) #fair coin
(binom.l2<-dbinom(3,4,0.8)) #tric coin

4*calcul.l1
4*calcul.l2

(binom.p1<-binom.l1/(binom.l1+binom.l2))
(binom.p2<-binom.l2/(binom.l1+binom.l2))

#Check against the previous attempts
calcul.p1
calcul.p2

simul.p1
simul.p2

(onemore.l1<-dbinom(3,5,0.5))
(onemore.l2<-dbinom(3,5,0.8))

(onemore.p1<-onemore.l1/(onemore.l1+onemore.l2))
(onemore.p2<-onemore.l2/(onemore.l1+onemore.l2))

binom.p1
binom.p2

#And now we toss "tails". That is 0 "heads" out of 1 toss.
(onetails.p1<-dbinom(0,1,0.5))
(onetails.p2<-dbinom(0,1,0.8))

#Like adding *0.5 or *0.2 to our row of
0.8*0.2*0.8*0.8
#and get
0.8*0.2*0.8*0.8*0.2

#for the tric coin and 
0.5*0.5*0.5*0.5
#and get
0.5*0.5*0.5*0.5*0.5

#Look how it works:
#With likelihood
(updated.l1<-binom.l1*dbinom(0,1,0.5))
(updated.l2<-binom.l2*dbinom(0,1,0.8))

(updated.p1<-updated.l1/(updated.l1+updated.l2))
(updated.p2<-updated.l2/(updated.l1+updated.l2))

#With probability
(updated.l1<-binom.p1*dbinom(0,1,0.5))
(updated.l2<-binom.p2*dbinom(0,1,0.8))

(updated.p1<-updated.l1/(updated.l1+updated.l2))
(updated.p2<-updated.l2/(updated.l1+updated.l2))

#And it is the same as unupdated "from scratch" version
onemore.p1
onemore.p2

#And we are there - we are at gates of Bayes' theorem.
# P(H)=P(D|H)P(H)/P(D)

#If we have two hypotheses like now, we can say that it is
#   P(H1)=(P(D|H1)P(H1))/(P(D|H1)P(H1)+P(D|H2)P(H2))
#   P(H2)=(P(D|H2)P(H2))/(P(D|H1)P(H1)+P(D|H2)P(H2))

#See? Numerators are different, denominators are the same. 
#numerators
(l1<-dbinom(3,4,0.5)*0.1) 
(l2<-dbinom(3,4,0.8)*0.9)

#divide them with a common denominator, which is just the product of all numerators
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#And after the additional "tails" it looks like this
(l1<-dbinom(0,1,0.5)*p1) 
(l2<-dbinom(0,1,0.8)*p2) 
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Formally we just did this:

#numerators
(l1<-dbinom(3,4,0.5)*0.5) 
(l2<-dbinom(3,4,0.8)*0.5)

#divide them with a common denominator, which is just the product of all numerators
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Plus the additional "tails", our favourite updating:
(l1<-dbinom(0,1,0.5)*p1) 
(l2<-dbinom(0,1,0.8)*p2) 
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#For the sake of absurdity:
(l1<-dbinom(1,1,0.5)*0.5) 
(l2<-dbinom(1,1,0.8)*0.5)
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Second toss (tails) 
(l1<-dbinom(0,1,0.5)*p1) 
(l2<-dbinom(0,1,0.8)*p2)
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Third toss (heads) 
(l1<-dbinom(1,1,0.5)*p1) 
(l2<-dbinom(1,1,0.8)*p2)
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Fourth toss (heads) 
(l1<-dbinom(1,1,0.5)*p1) 
(l2<-dbinom(1,1,0.8)*p2)
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Fifth toss (tails) 
(l1<-dbinom(0,1,0.5)*p1) 
(l2<-dbinom(0,1,0.8)*p2)
(p1<-l1/(l1+l2))
(p2<-l2/(l1+l2))

#Fun useless fact
#STUPIDEST SAMPLER EVER

#The question is what if we wish to model the function P(H) not as relative proportion of probability of two discrete alternative hypotheses
#What would you do?

(Hp<-seq(0,1,by=0.1))
(Hp<-seq(0,1,by=0.01))
(prior<-rep(prior.p,length(Hp)))

#prior
(prior.p<-1/length(Hp)) #prior probability of each hypothesis is slightly under 1 percent
prior<-rep(prior.p,length(Hp))
  
plot(Hp,prior,type="l",xaxs="i",yaxs="i",ylim=c(0,1),col=2)

#You will be fine

#But let us define and use it anyway to get used to the idea of omnipresent
prior<-rep(1,length(Hp))
plot(Hp,prior,type="l",xaxs="i",yaxs="i",ylim=c(0,1.1),col=2)

(l<-dbinom(3,4,Hp)) 

(numerators<-l*prior)

sum(numerators)

(post<-numerators/sum(numerators))

#There is your graphical representation of the continuum
plot(Hp,post,type="l",lwd=2,col=4)

#We named it post for posterior, but remember that every posterior can become prior of the next round.
post<-(dbinom(0,1,Hp)*post)/sum((dbinom(0,1,Hp)*post))
plot(Hp,post,type="l",lwd=2,col=4)
#See that the peak is at the observed proportion, since we started from the perfectly flat prior
abline(v=3/5,col=2)

#Is this the end?
#But imagine you had some system that - unlike coin - has more than one parameter.

#Please: Some examples from the audience.

#My example
#dice
rep(1/6,6)

#Unfair dice might be
(6:1)/sum(6:1)

#But there are many more. 1 vs the rest behaves like a coin, it can fall with probability
(p1<-seq(0,1,by=0.01))

#The rest? You just take whatever is left and divide it with the same sensitivity
(p2<-lapply(p1,function(x){seq(0,1-x,0.01)}))
d<-data.frame(p1=rep(p1,sapply(p2,length)),p2=unlist(p2))
nrow(d)

#The same goes for p3, only I divide the remnant after summing p1 and p2
p3<-lapply(rowSums(d),function(x){seq(0,1-x,0.01)})
d<-data.frame(p1=rep(d$p1,sapply(p3,length)),p2=rep(d$p2,sapply(p3,length)),p3=unlist(p3))
nrow(d)

p4<-lapply(rowSums(d),function(x){seq(0,1-x,0.01)})
d<-data.frame(p1=rep(d$p1,sapply(p4,length)),
              p2=rep(d$p2,sapply(p4,length)),
              p3=rep(d$p3,sapply(p4,length)),
              p4=unlist(p4))
nrow(d)

#Now I have over 4 million hypothesis and I am not done
(Hypotheses<-expand.grid(p1=Hp,p2=Hp))
nrow(Hypotheses)
#basically
length(Hp)*length(Hp) #this much

#If I had three such parameters
(Hypotheses<-expand.grid(p1=Hp,p2=Hp,p3=Hp))
nrow(Hypotheses)

length(Hp)^1 #one parameter between 0 and 1 with the same precision
length(Hp)^2 #two parameters
length(Hp)^3 #three parameters
length(Hp)^4 #four parameters
length(Hp)^5 #five parameters (well over 100 milliard hypotheses)

#Who invents nice scalable example for this (the problem with dice is mutual dependence of likelihood of sides), such example that we use from the next year on, gets the credit for this course for free!

length(Hp)^(10*10*195) #Which is beyond the imagination of almost any computer

#For this reason, people almost always use not regularly spaced, but randomly drawn hypotheses (which, in effect, frequently leads to very similar ends).


#Let us program the stupidest random sampler ever!
Hpr<-runif(10000,min=0,max=1) #function runif generates random numbers from a flat uniform distribution
l<-dbinom(3,4,Hpr)

samples<-sample(Hpr,1000,replace=T,prob=l)
str(samples)
hist(samples)

#If we want to approximate this many discrete "observations" by a curve, we will need KDE - Kernel Density Estimation
dens<-density(samples)
plot(Hp,post*100,type="l",lwd=2,col=4)
lines(dens$x,dens$y,lwd=2,col=3)

#Power of logarithm
log(exp(5))
exp(log(3))

#When our ancestors needed to multiply 3 large numbers
N<-c(225698,556877988,3546899)
#They took their logarithms
(logN<-log(N))
#Added the logarithms together
(sumN<-sum(logN))
#And then possibly raised e (natural logarithm base) to the resulting sum
exp(sumN)

#Now we have computers
prod(N)

#But logarithms are still very useful, let me demonstrate:

#First I will define a neater plotting function than the default one
myplot<-function(x,y,y2=NULL,rounding=2,mycol="#2288FF",...){
  namx<-deparse(substitute(x))
  namy<-deparse(substitute(y))
  
  par(mar=c(4,4,4,1),mgp=c(2.2,1,0))
  plot(x,y,col=mycol,pch=16,xlab=namx,ylab=namy,...)
}

#Here I start by uploading some data
d<-read.table("gdppopdata.txt" ,sep="\t",header=T)
#There are some NAs for bigger units, that I discard
d<-d[!is.na(d$population),]

#See what we got here
myplot(d$population,d$totalGDP)
text(d$population,d$totalGDP,d$abr,pos=3,xpd=T)

myplot(log(d$population),log(d$totalGDP))
text(log(d$population),log(d$totalGDP),d$abr,pos=3,xpd=T)

#If you investigate the description of the log function, you discover that you can modify the logarithm base.
?log

myplot(log(d$population,10),log(d$totalGDP,10))

#GDP is in millions, population in thousands
#lets do
myplot(log(d$population*1000,10),log(d$totalGDP*1000000,10))
points(log(d$population*1000,10)[d$abr=="CZE"],log(d$totalGDP*1000000,10)[d$abr=="CZE"],col=2,pch=16)
abline(v=log(d$population*1000,10)[d$abr=="CZE"],col=2,lty=2)
abline(h=log(d$totalGDP*1000000,10)[d$abr=="CZE"],col=2,lty=2)
#Czech republic is the red point

#We can have a better solution.
better<-d
better$population<-log(d$population*1000,1.1)
better$totalGDP<-log(d$totalGDP*1000000,1.1)

#If we plot it and find Czech republic on the graph
myplot(better$population,better$totalGDP)
points(better$population[better$abr=="CZE"],better$totalGDP[better$abr=="CZE"],col=2,pch=16)
abline(v=better$population[better$abr=="CZE"],col=2,lty=2)
abline(h=better$totalGDP[better$abr=="CZE"],col=2,lty=2)

#We can easily talk about differences between countries on this scale
(cze<-better$totalGDP[better$abr=="CZE"])
(usa<-better$totalGDP[better$abr=="USA"])
#The numbers seem rather similar

(dif<-usa-cze)
#The difference between Czechia and USA is 41. That means to get USA GDP, we would have to multiply our current GDP by 1.1 41 times (that means increase our GDP by 10% than increase the result by 10% again and so on and so on...)

1.1*1.1
1.1*1.1*1.1
1.1^3
1.1^dif

multip<-1.1^dif

d$totalGDP[d$abr=="CZE"]

#See, the numbers are the same
d$totalGDP[d$abr=="CZE"]*multip
d$totalGDP[d$abr=="USA"]

#But maybe such large numbers are, again, not very good for our imagination. What to do? Cn we easily try out conversions between different bases?
log(1.1^dif,base=10)
log(1.1^dif,base=2) 

best<-d
best$population<-log(d$population*1000,2)
best$totalGDP<-log(d$totalGDP*1000000,2)

#If we plot it and find Czech republic on the graph
myplot(best$population,best$totalGDP)
points(best$population[best$abr=="CZE"],best$totalGDP[best$abr=="CZE"],col=2,pch=16)
abline(v=best$population[best$abr=="CZE"],col=2,lty=2)
abline(h=best$totalGDP[best$abr=="CZE"],col=2,lty=2)

#What is the best visualization of data that illustrate a multiplicative processes and relationships between them?
#In my opinion it the best to set some threshold as 0 and reference the units of multiplication towards this threshold.
#All it takes is to subtract the threshold value from all logarithm scores
best$population<-best$population-best$population[best$abr=="CZE"]
best$totalGDP<-best$totalGDP-best$totalGDP[best$abr=="CZE"]

#Now Czech republic is at 0 and all other scores represent *2 (positive) or /2 (negative) changes in GDP or population size 
myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
abline(v=0,col=2,lty=2)
abline(h=0,col=2,lty=2)

#So how to parametrize such a system into a set of plausible hypotheses?
myplot(best$population,best$totalGDP,type="n")
#How might such data arise?

points(0,0,col=2,pch=16)

#But sholud that country be there?
points(0,1,col=3,pch=16)
#What does it mean?
#1 on base-2 logarithmic scale means that Czech GDP should be twice as big.

curve(exp(-x^2),xlim=c(-3,3),col=4)

a<-1
sigma<-0.5

myplot(best$population,best$totalGDP,type="n")
points(0,a,col=3,pch=16)
#Let us generate 50 possible Czechias from this distribution and put them into the plot.
(newczech<-rnorm(50,mean=a,sd=sigma))
points(rep(0,50),newczech,col="#88888820",pch=16)
points(0,0,col=2,pch=16)
#The red dot - the GDP=0 in the data is at the very edge of the generated values, but if we check the value of probability density function, we see, that it is indeed quite possible to draw it from such a distribution.
dnorm(0,mean=a,sd=sigma)

#It is obviously much more likely to draw numbers close to the mean.
dnorm(1,mean=a,sd=sigma)

b<- -1

#So now there are expected value per log population size
abline(a=a,b=b,col=3)

#And around it I can generate some random data: 100 new countries
n<-100
rpop<-runif(n,-4,6) #We have their random populations uniformly distributed between -4 and 6
#We calculate the expected GDP for each country with the simple linear equation
mu<-a+b*rpop

#And generate random deviation around each expected number
rGDP<-rnorm(n,mu,sigma)

points(rpop,rGDP,col="#888888",pch=16)
points(best$population,best$totalGDP,col=4,pch=16)

#Let us use our STUPIDEST SAMPLER EVER to get the distribution of likely triplets of parameter values
#This is how many random hypotheses I generate
nH<-100000
a<-rnorm(nH,mean=0,sd=1)
b<-rnorm(nH,mean=0,sd=1)
sigma<-rexp(nH,rate=1)

hypotheses<-data.frame(a,b,sigma)

#This is how I would calculate likelihoods of all observations in my data:
#I will demonstrate on first hypothesis
i<-1
h<-hypotheses[i,]
per.point<-dnorm(best$totalGDP,h$a+h$b*best$population,h$sigma) #likelihood per point
prod(per.point)

#The data likelihood for this hypothesis is extremely low
h

#Maybe i will be more lucky elsewhere. I will turn the likelihood into a function:
mylikeli<-function(i){
  h<-hypotheses[i,]
  per.point<-dnorm(best$totalGDP,h$a+h$b*best$population,h$sigma) #likelihood per point
  return(prod(per.point))
}

#Calculate P(d|H) for each random hypothesis
hypotheses$data.l<-sapply(1:nrow(hypotheses),mylikeli)

#There are some that allow for the generation of data as they are
str(sort(hypotheses$data.l,decreasing=T))

sampled.h<-sample(1:nH,1000,replace=T,prob=hypotheses$data.l)
samples<-hypotheses[sampled.h,]

head(samples)

#I have a set of plausible hypotheses.
pnorm(2)-pnorm(-2)

myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
abline(a=samples[1,]$a,b=samples[1,]$b)
abline(a=samples[1,]$a+samples[1,]$sigma*2,b=samples[1,]$b,lty=2)
abline(a=samples[1,]$a-samples[1,]$sigma*2,b=samples[1,]$b,lty=2)

#This is second hypothesis
myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
abline(a=samples[2,]$a,b=samples[2,]$b)
abline(a=samples[2,]$a+samples[2,]$sigma*2,b=samples[2,]$b,lty=2)
abline(a=samples[2,]$a-samples[2,]$sigma*2,b=samples[2,]$b,lty=2)

#I will for now plot 50 likely hypotheses (but without the sigma parameter fro better clarity)
myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
for(i in 1:50){
  abline(a=samples[i,]$a,b=samples[i,]$b,col="#80808080")
}

#We can clearly see that the hypotheses more or less agree on some corridor. We can formalize it even better - not with the bunch of rod but by a region. You can construct it like this:
x<-seq(-7,7,0.01)
y<-sapply(1:nrow(samples),function(i){samples[i,]$a+samples[i,]$b*x})
str(y)
muy<-apply(y,1,mean)
CIy<-apply(y,1,PI,prob=0.95)

#This is compatibility interval
myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
shade(CIy,x)

#There is an alternative to PI, which is just a method that cuts off symmetric quantiles, HPDI
HPDIy<-apply(y,1,HPDI,prob=0.95)
myplot(best$population,best$totalGDP)
points(0,0,col=2,pch=16)
shade(HPDIy,x)
#very very frequently these two methods givealmost identical results.

#It is already clear that Czechia is well within the corridor of likely area close to the expected mean
summary(samples$a)
PI(samples$a,prob=0.95) #With probability 95% a (difference between expected GDP of Czechia and the realized total GDPis close to 0).
#If we use the PI function (first function from the rethinking package that we use), it does not give 95% but 89% CI 
#Parameter b
(mu.b<-summary(samples$b))
(CI.b<-PI(samples$b,prob=0.95))

#Per doubling the population (move one step right along the x axis), the GDP is expected to grow by factor of approx
2^mu.b["Mean"]

#95% CI is
2^CI.b

#So we clearly see that GDP grows with population

#We can summarize parameters nicely graphicaly
plot(NULL,xlim=c(-2,2),ylim=c(3.5,0.5),xlab="parameter value",ylab="",yaxt="n")
axis(2,at=c(1,2,3),labels=c("a","b","sigma"),las=2)
abline(h=1:3)
toplot<-samples$a
dens<-density(toplot)
sc<-0.1 #You need to define a scaler not to draw to big density plots
polygon(dens$x,1-dens$y*sc,col=4)
lines(PI(toplot,prob=0.95),c(1,1),lwd=3)
points(mean(toplot),1,lwd=2,cex=1.2,bg=0,pch=21)

#I can pack this to a nice function
drawDist<-function(toplot,y,sc=0.1){
  dens<-density(toplot)
  polygon(dens$x,y-dens$y*sc,col=4)
  lines(PI(toplot,prob=0.95),c(y,y),lwd=3)
  points(mean(toplot),y,lwd=2,cex=1.2,bg=0,pch=21)
}

#Again with all parameters
plot(NULL,xlim=c(-2,2),ylim=c(3.5,0.5),xlab="parameter value",ylab="",yaxt="n")
axis(2,at=c(1,2,3),labels=c("a","b","sigma"),las=2)
abline(h=1:3)
drawDist(samples$a,y=1)
drawDist(samples$b,y=2)
drawDist(samples$sigma,y=3)
abline(v=seq(-2,2,1),lty=2)

#The last thing that we can plot is the correlation between parameter values across samples.
pairs(samples[,1:3])

#It does not look like a complicated intercorrelated posterior.

#Stan Ulam - the best sampler
clean.d<-list(pop=best$population,
              gdp=best$totalGDP)

set_ulam_cmdstan(FALSE) #maybe you do not need to do that

model<-ulam(alist(
  gdp ~ dnorm(a+b*pop,sigma),
  a ~ dnorm(0,1),
  b ~ dnorm(0,1),
  sigma ~ dexp(1)
),data=clean.d,chains=4,cores=4)

#It is advisable to save once sampled models - especially the big ones
save(model,file="sampled/first.ulam.model.Rdata")
load("sampled/first.ulam.model.Rdata")

#I can very quickly summarize this with another rethinking function precis
precis(model) #It is almost identical to the stupidest sampler

#Extracting samples is easy
post<-extract.samples(model)

#TASK 1: Visualize the parameter values from the posterior (object post).  
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 1 

#TASK 2: Draw the prediction line and the 99% Percentile Compatibility interval around it 
#If you struggle, you can find the solution in TASK_SOLUTIONS.R line 11 

#We can draw the predictions of mean GDP also on the original scale
#We need to destandardize the generated population sizes
addpop<-log(d$population[best$abr=="CZE"]*1000,base=2)

#Check that it is the same
2^(best$population+addpop)/1000
d$population 

myplot(d$population,d$totalGDP)

#destandardization of gdp will need Czech GDP base2 logarithm
addgdp<-log(d$totalGDP[best$abr=="CZE"]*1000000,base=2)

#Generate x and predict y
x<-seq(-7,8,0.01)
y<-sapply(1:length(post$a),function(i){post$a[i]+post$b[i]*x})

#Destandardize
orig.scale.x<-2^(x+addpop)/1000
orig.scale.y<-2^(y+addgdp)/1000000

#Plot
muy<-apply(orig.scale.y,1,mean)
CIy<-apply(orig.scale.y,1,PI,prob=0.95)
lines(orig.scale.x,muy)
shade(CIy,orig.scale.x)

#The conversion between multiplicative and additive frames comes useful, because computers are stupid and they do not represent small numbers very well - look:
A<-rep(c(0.001,0.0005,0.0003),100)
B<-rep(c(0.001,0.0005,0.0003),50)
A
B

#Taking their products returns simply 0
prod(A)
prod(B)

#But if we log them
(logA<-log(A,10))
(logB<-log(B,10))
#And sum the logs
(sumA<-sum(log(A,10)))
(sumB<-sum(log(B,10)))
#We can clearly see, that the resulting probability of B product is much higher than resulting probability of A
