##############
# WRITE 4 functions for calculating a program's cost and revenue
###############
# 1. simp_cost (fixed and variable NON-VACCINE costs)
# 2. simp_rev (total gross program revenue)
# 3. simp_vac (vaccine only costs)
# 4. net_rev (a wrapper for the previous 3, calculating TOTAL NET REVENUE)

# 1. simp_cost() ##########
simp_cost <-function(n,
                     imm_rate = 100,
                     start_up = 47135.69){
  variable <- n * 4.52 * (imm_rate * 0.01)
  total_cost <- start_up+variable
  return(total_cost)
}



# 2. simp_rev() ############
simp_rev <-function(n_private, # number of kids with private
                    n_nonprivate,
                    suc_bill_private =80.35,#this is defualt of AC program
                    suc_bill_nonprivate =64.92,
                    reim_private = 47.05,
                    reim_nonprivate = 5,
                    imm_rate_private = 100,
                    imm_rate_nonprivate = 100){ #deafault
  
  # private revenue
  private <- # total private revenue is equal to
    n_private * # number of privately insured kids
    (suc_bill_private/100) * # times the percentage of private kids who will be suc billed
    reim_private  * # times the reimbursement rate
    (imm_rate_private/100) # times the private immunization rate
  
  # public revenue
  public <- #total public revenue
    n_nonprivate *
    (suc_bill_nonprivate/100) *
    reim_nonprivate *
    (imm_rate_nonprivate/100)
  
  # total revenue
  total <- private+public
  
  # spit back at the user
  return(total)
} 
####################


# 3. simp_vac ############### 
#making function for vaccination cost of program 
simp_vac <-function(n_private, 
                    n_nonprivate,
                    imm_rate_private,
                    imm_rate_nonprivate,
                    vac_cost_private=17.50, #dfault
                    vac_cost_nonprivate=0){ #dfault 
  
  # private vaccine cost
  private <-n_private *
    (imm_rate_private/100) *
    vac_cost_private
  
  # public vaccine cost
  public <-n_nonprivate *
    (imm_rate_nonprivate/100) *
    vac_cost_nonprivate
  
  # total revenue
  total <- private+public
  
  # spit back at the user
  return(total)
  
}

# WRAP FUNCTIONS INTON ONE MASTER NET REV FUNCTION
# only using this for mapping
net_rev <- function(number_private = 20000,
                    number_nonprivate = 20000,
                    ir = 100,
                    collaborative = TRUE,
                    reim_np = 5,
                    reim_p = 47.05,
                    suc_np = 64.92,
                    suc_p = 80.35){
  
  if(collaborative){
    adjusted_start_up <- (number_private + number_nonprivate) * 1.53
  } else {
    adjusted_start_up <- (number_private + number_nonprivate) * 3.23
    
  }
  
  cost <- simp_cost(n = number_private + number_nonprivate,
                    imm_rate = ir,
                    start_up = adjusted_start_up) 
  
  vac_cost <- simp_vac(n_private = number_private, n_nonprivate = number_nonprivate,
                       imm_rate_private = ir,
                       imm_rate_nonprivate = ir)
  
  rev <- simp_rev(n_private = number_private, n_nonprivate = number_nonprivate,
                  imm_rate_private = ir,
                  imm_rate_nonprivate = ir,
                  reim_nonprivate = reim_np,
                  reim_private = reim_p,
                  suc_bill_private = suc_p,
                  suc_bill_nonprivate = suc_np)
  
  net_profit <- rev - vac_cost - cost
  return(net_profit)
}
