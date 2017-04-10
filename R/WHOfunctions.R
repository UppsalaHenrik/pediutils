# These functions are distributed together with the WHO data.
# 
# References:
# WHO Multicentre Growth Reference Study Group (2006). WHO Child Growth 
# Standards: Length/height-for-age, weight-for-age, weight-for-length, 
# weight-for-height and body mass index-for-age: Methods and development. 
# Geneva: World Health Organization; pp 312. (web site: 
# http://www.who.int/childgrowth/publications/en/ )
#
# WHO Multicentre Growth Reference Study Group (2007). WHO Child Growth 
# Standards: Head circumference-for-age, arm circumference-for-age, 
# triceps skinfold-for-age and subscapular skinfold-for-age: Methods 
# and development. Geneva: World Health Organization; pp 217. (web site: 
# http://www.who.int/childgrowth/publications/en/ )


############ 0-5 years

######################################################################################
### Function for calulating individual Length-for-age z-scores
######################################################################################

calc.zlen.upto5<-function(mat,lenanthro){
  
  for(i in 1:length(mat$age.days)) {
    
    if(!is.na(mat$age.days[i])) {
      
      l.val<-lenanthro$l[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
      m.val<-lenanthro$m[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
      s.val<-lenanthro$s[lenanthro$age==mat$age.days[i] & lenanthro$sex==mat$sex[i]]
      mat$zlen[i]<-(((mat$clenhei[i]/m.val)^l.val)-1)/(s.val*l.val)	
      
    }	else mat$zlen[i]<- NA
    
  }
  return(mat$zlen[1])
}

######################################################################################
### Function for calulating individual Weight-for-age z-scores
######################################################################################

calc.zwei.upto5<-function(mat,weianthro){
  
  for(i in 1:length(mat$age.days)) {
    
    if(!is.na(mat$age.days[i]) & mat$oedema[i]!="y") {
      
      l.val<-weianthro$l[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
      m.val<-weianthro$m[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
      s.val<-weianthro$s[weianthro$age==mat$age.days[i] & weianthro$sex==mat$sex[i]]
      
      mat$zwei[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zwei[i]) & mat$zwei[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zwei[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zwei[i]) & mat$zwei[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zwei[i]<- (-3)+((mat$weight[i]-sd3neg)/sd23neg)
      }
      
    } else mat$zwei[i]<-NA
  }
  return(mat$zwei[1])
}

######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################

calc.zbmi.upto5<-function(mat,bmianthro){
  
  for(i in 1:length(mat$age.days)) {
    
    if(!is.na(mat$age.days[i]) & mat$age.days[i]>=0 & mat$age.days[i]<=1856 & mat$oedema[i]!="y") {
      
      l.val<-bmianthro$l[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
      m.val<-bmianthro$m[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
      s.val<-bmianthro$s[bmianthro$age==mat$age.days[i] & bmianthro$sex==mat$sex[i]]
      
      mat$zbmi[i]<-(((mat$cbmi[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zbmi[i]) & mat$zbmi[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zbmi[i]<- 3+((mat$cbmi[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zbmi[i]) & mat$zbmi[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zbmi[i]<- (-3)+((mat$cbmi[i]-sd3neg)/sd23neg)
      }
      
    } else mat$zbmi[i]<-NA
    
  }
  
  return(mat$zbmi[1])
}


############ 5+ years


######################################################################################
### Function for calculating individual height-for-age z-scores
######################################################################################

calc.zhfa.over5<-function(mat,hfawho2007){
  
  for(i in 1:length(mat$age.mo)) {
    
    if(!is.na(mat$age.mo[i]) & mat$age.mo[i]>=61 & mat$age.mo[i]<229) {
      
      ### Interpolated l,m,s values
      
      low.age<-trunc(mat$age.mo[i])
      upp.age<-trunc(mat$age.mo[i]+1)
      diff.age<-(mat$age.mo[i]-low.age)
      
      if(diff.age>0) {																																																													
        l.val<-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$l[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
        m.val<-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$m[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
        s.val<-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]+diff.age*( hfawho2007$s[hfawho2007$age==upp.age & hfawho2007$sex==mat$sex[i]]-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]] )
      } else {
        l.val<-hfawho2007$l[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
        m.val<-hfawho2007$m[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
        s.val<-hfawho2007$s[hfawho2007$age==low.age & hfawho2007$sex==mat$sex[i]]
      }
      mat$zhfa[i]<-(((mat$height[i]/m.val)^l.val)-1)/(s.val*l.val)	
      
    }	else mat$zhfa[i]<- NA
    
  }
  return(mat$zhfa[1])
}

######################################################################################
### Function for calculating individual weight-for-age z-scores
######################################################################################

calc.zwei.over5<-function(mat,wfawho2007){
  
  for(i in 1:length(mat$age.mo)) {
    
    if(!is.na(mat$age.mo[i])  & mat$age.mo[i]>=61 & mat$age.mo[i]<121 & mat$oedema[i]!="y") {
      
      ### Interpolated l,m,s values
      
      low.age<-trunc(mat$age.mo[i])
      upp.age<-trunc(mat$age.mo[i]+1)
      diff.age<-(mat$age.mo[i]-low.age)
      
      if(diff.age>0) {																																																													
        l.val<-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$l[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
        m.val<-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$m[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
        s.val<-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]+diff.age*( wfawho2007$s[wfawho2007$age==upp.age & wfawho2007$sex==mat$sex[i]]-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]] )
      } else {
        l.val<-wfawho2007$l[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
        m.val<-wfawho2007$m[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
        s.val<-wfawho2007$s[wfawho2007$age==low.age & wfawho2007$sex==mat$sex[i]]
      }
      
      mat$zwfa[i]<-(((mat$weight[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zwfa[i]) & mat$zwfa[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zwfa[i]<- 3+((mat$weight[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zwfa[i]) & mat$zwfa[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zwfa[i]<- (-3)+((mat$weight[i]-sd3neg)/sd23neg)
      }
      
    } else mat$zwfa[i]<-NA
  }
  return(mat$zwfa[1])
}

######################################################################################
### Function for calulating individual BMI-for-age z-scores
######################################################################################

calc.zbmi.over5<-function(mat,bfawho2007){
  
  for(i in 1:length(mat$age.mo)) {
    
    if(!is.na(mat$age.mo[i]) & mat$age.mo[i]>=61 & mat$age.mo[i]<229 & mat$oedema[i]!="y") {
      
      ### Interpolated l,m,s values
      
      low.age<-trunc(mat$age.mo[i])
      upp.age<-trunc(mat$age.mo[i]+1)
      diff.age<-(mat$age.mo[i]-low.age)
      
      if(diff.age>0) {																																																													
        l.val<-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$l[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
        m.val<-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$m[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
        s.val<-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]+diff.age*( bfawho2007$s[bfawho2007$age==upp.age & bfawho2007$sex==mat$sex[i]]-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]] )
      } else {
        l.val<-bfawho2007$l[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
        m.val<-bfawho2007$m[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
        s.val<-bfawho2007$s[bfawho2007$age==low.age & bfawho2007$sex==mat$sex[i]]
      }
      
      mat$zbfa[i]<-(((mat$cbmi[i]/m.val)^l.val)-1)/(s.val*l.val)
      if(!is.na(mat$zbfa[i]) & mat$zbfa[i]>3) {
        sd3pos<- m.val*((1+l.val*s.val*3)^(1/l.val))
        sd23pos<- sd3pos- m.val*((1+l.val*s.val*2)^(1/l.val))
        mat$zbfa[i]<- 3+((mat$cbmi[i]-sd3pos)/sd23pos)
      }
      if(!is.na(mat$zbfa[i]) & mat$zbfa[i]< (-3)) {
        sd3neg<- m.val*((1+l.val*s.val*(-3))**(1/l.val))
        sd23neg<- m.val*((1+l.val*s.val*(-2))**(1/l.val))-sd3neg
        mat$zbfa[i]<- (-3)+((mat$cbmi[i]-sd3neg)/sd23neg)
      }
      
    } else mat$zbfa[i]<-NA
    
  }
  
  return(mat$zbfa[1])
}
