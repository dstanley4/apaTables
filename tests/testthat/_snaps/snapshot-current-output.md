# apa.cor.table console output is stable

    Code
      print(apa.cor.table(attitude))
    Output
      
      
      Table 1 
      
      Descriptive Statistics and Correlations
       
      
        Variable      N  M     SD    1           2           3           4          
        1. rating     30 64.63 12.17                                                
                                                                                    
                                                                                    
                                                                                    
        2. complaints 30 66.60 13.31 .83**                                          
                                     [.66, .91]                                     
                                     p < .001                                       
                                                                                    
        3. privileges 30 53.13 12.24 .43*        .56**                              
                                     [.08, .68]  [.25, .76]                         
                                     p = .019    p = .001                           
                                                                                    
        4. learning   30 56.37 11.74 .62**       .60**       .49**                  
                                     [.34, .80]  [.30, .79]  [.16, .72]             
                                     p < .001    p < .001    p = .006               
                                                                                    
        5. raises     30 64.63 10.40 .59**       .67**       .45*        .64**      
                                     [.29, .78]  [.41, .83]  [.10, .69]  [.36, .81] 
                                     p < .001    p < .001    p = .014    p < .001   
                                                                                    
        6. critical   30 74.77 9.89  .16         .19         .15         .12        
                                     [-.22, .49] [-.19, .51] [-.22, .48] [-.25, .46]
                                     p = .409    p = .321    p = .438    p = .542   
                                                                                    
        7. advance    30 42.93 10.29 .16         .22         .34         .53**      
                                     [-.22, .49] [-.15, .54] [-.02, .63] [.21, .75] 
                                     p = .413    p = .233    p = .063    p = .003   
                                                                                    
        5          6          
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
                              
        .38*                  
        [.02, .65]            
        p = .040              
                              
        .57**      .28        
        [.27, .77] [-.09, .58]
        p < .001   p = .129   
                              
      
      Note. N = number of cases. M = mean. SD = standard deviation.
      Values in square brackets indicate the 95% confidence interval.
       * indicates p < .05. ** indicates p < .01.
       
      

# apa.reg.table console output is stable

    Code
      print(apa.reg.table(blk1, blk2, table.number = 2))
    Output
      
      
      Table 2 
      
      Hierarchical Multiple Regression Predicting Sales
       
      
         Predictor       b       b_95%_CI beta  beta_95%_CI Unique_R2 Unique_95%_CI
           Model 1                                                                 
       (Intercept) 41.12** [22.72, 59.53]                                          
           adverts  0.09**   [0.07, 0.10] 0.52 [0.44, 0.61]     .27**    [.18, .36]
           airplay  3.59**   [3.02, 4.15] 0.55 [0.46, 0.63]     .29**    [.20, .38]
                                                                                   
                                                                                   
                                                                                   
           Model 2                                                                 
       (Intercept)  -26.61 [-60.83, 7.60]                                          
           adverts  0.08**   [0.07, 0.10] 0.51 [0.43, 0.59]     .26**    [.17, .34]
           airplay  3.37**   [2.82, 3.92] 0.51 [0.43, 0.60]     .25**    [.17, .33]
           attract 11.09**  [6.28, 15.89] 0.19 [0.11, 0.27]     .04**    [.00, .07]
                                                                                   
                                                                                   
                                                                                   
           r             Fit         Delta_Fit
                                              
                                              
       .58**                                  
       .60**                                  
                 R2 = .629**                  
             95% CI[.55,.69]                  
                                              
                                              
                                              
       .58**                                  
       .60**                                  
       .33**                                  
                 R2 = .665** Delta R2 = .035**
             95% CI[.59,.72]  95% CI[.00, .07]
                                              
      
      Note. N = 200. b = unstandardized regression weight. beta = standardized regression weight. Unique R2 = semipartial correlation squared. r = zero-order correlation. CI = confidence interval.  
      * indicates p < .05. ** indicates p < .01.
       
      

# apa.aov.table console output is stable

    Code
      print(apa.aov.table(lm_output, table.number = 4))
    Output
      
      
      Table 4 
      
      ANOVA Results for Libido
       
      
         Predictor    SS df    MS     F    p partial_eta2 CI_90_partial_eta2
       (Intercept) 24.20  1 24.20 12.31 .004                                
              dose 20.13  2 10.06  5.12 .025          .46         [.04, .62]
             Error 23.60 12  1.97                                           
      
      Note. SS = Sum of squares. df = degrees of freedom. MS = mean square. CI indicates the confidence interval for partial eta-squared. 
      

# apa.1way.table console output is stable

    Code
      print(apa.1way.table(iv = dose, dv = libido, data = viagra, table.number = 1))
    Output
      
      
      Table 1 
      
      Descriptive Statistics for Libido by Dose 
      
            dose    M   SD
         Placebo 2.20 1.30
        Low Dose 3.20 1.30
       High Dose 5.00 1.58
      
      Note. M = mean. SD = standard deviation.
       
      

# apa.2way.table console output is stable

    Code
      print(apa.2way.table(iv1 = gender, iv2 = alcohol, dv = attractiveness, data = goggles,
        table.number = 3))
    Output
      
      
      Table 3 
      
      Descriptive Statistics for Attractiveness in a 2(Gender) by 3(Alcohol) Design 
      
              alcohol                                  
                 None       2 Pints       4 Pints      
       gender       M    SD       M    SD       M    SD
       Female   60.62  4.96   62.50  6.55   57.50  7.07
         Male   66.88 10.33   66.88 12.52   35.62 10.84
      
      Note. M = mean. SD = standard deviation. 
      

# apa.d.table console output is stable

    Code
      print(apa.d.table(iv = dose, dv = libido, data = viagra))
    Output
      
      
      Table 1 
      
      Means, Standard Deviations, and d-Values for Libido by Dose
       
      
        Variable     M    SD   1             2            
        1. Placebo   2.20 1.30                            
                                                          
        2. Low Dose  3.20 1.30 0.77                       
                               [-0.55, 2.04]              
                                                          
        3. High Dose 5.00 1.58 1.93          1.24         
                               [0.34, 3.44]  [-0.17, 2.59]
                                                          
      
      Note. M indicates mean. SD indicates standard deviation. d-values are estimates calculated using formulas 4.18 and 4.19
      from Borenstein, Hedges, Higgins, & Rothstein (2009). d-values not calculated if unequal variances prevented pooling.
      Values in square brackets indicate the 95% confidence interval for each d-value. 
       
      

