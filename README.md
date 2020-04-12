# RotationScheduler

The main function used to simulate transmission of COVID-19 in non-COVID hospital wards is called COVID_sim9. COVID_sim9 can be used to simulate the COVID-19 wards for various scheduling design choices, and can be used to compare the probability of team failure, for different scheduling design choices.

There are two main steps to using COVID_sim9 are:
1) Set a list of input parameters that are specific to the particular hospital setting and based on current knowledge about the SARS-COV-2 virus
2) Run the function for varying choices of scheduling design parameters

## Setting a list of input parameters (that do not depend on schedule design)

The input parameters can be set by running the following line of R code (with the numbers appropriately changed to the setting of interest, and based on the most current information about SARS-COV-2):


input_params <- list(T=180,
                     prob_medic_has_it_already=0.001, 
                     prob_patient_has_it_already=0.001,
                     prob_doctor_patient_transmission=0.15,
                     prob_patient_doctor_transmission=0.15,
                     prob_patient_nurse_transmission=0.2,
                     prob_nurse_patient_transmission=0.2,
                     days_out_doc=21,
                     prob_doc_critically_ill=0.07,
                     community_transmission_prob=0.0003,
                     inc_minus_latent=1,
                     npatients_init=15,
                     average_patient_stay=3,
                     lognormal_param1=1.63,
                     lognormal_param2=0.673,
                     days_past_symptom_patient_infective=1,
                     medic_medic_transmission_probability_factor=0.07,
                     maximum_hours_per_week_nurse=48,
                     n_attendings=3,
                     n_residents=6,
                     n_nurses=18,
                     n_attentings_at_once=1,
                     n_residents_at_once=2,
                     n_nurses_at_once=3)
                     
The parameters in the above list are defined as follows:

**T** is the number of days simulated.

**prob_medic_has_it_already** and **prob_patient_has_it_already** are the probabilities that a medic and a patient respictively have the virus on day 0 of the simulation. 

**prob_doctor_patient_transmission** is the probability that an infectious doctor will transmit the virus to a patient during one day of interactions with that patient. 

**prob_patient_doctor_transmission** is the probability that an infectious patient will transmit the virus to a doctor during one day of interactions with that doctor.

**prob_patient_nurse_transmission** is the probability that an infectious patient will transmit the virus to a nurse during a 4 hour block of time in which that nurse is caring for this patient.

**prob_nurse_patient_transmission** is the probability that an infectious nurse will transmit the virus to a patient during a 4 hour block of time in which that nurse is caring for this patient.

**days_out_doc** is the number of days an infected medic must stay at home until they are allowed to return to work. If days_out_doc=21, that means that a medic who started showing symptoms on day 10 can only return to work on day 31.

**prob_doc_critically_ill** is the probability that an infected medic becomes so ill, that they are unable to return to work for the entire duration of the simulation

**community_transmission_prob** is the daily probability that a medic is infected due to reasons other than the patients or the other medics on the team (e.g. elevator exposure, exposure at home, exposure at the supermarket or pharmacy, etc.)

**inc_minus_latent** is the number of days that someone is infectious before starting to show symptoms

**npatients_init** is the number of patients that the team is responsible for at the start of the simulation. Note that in our simulation the average number of patients present per day is equal to npatients_init.

**average_patient_stay** is the average number of days each patient stays in the hospital. Note that in our simulation, the duration each patient stay follows a geometric distribution with parameter 1/average_patient_stay.

**lognormal_param1** and **lognormal_param2** are the parameters that define a lognormal distribution. Whenever someone is infected in the simulation, their incubation period (the time from infection to symptoms) is drawn independently from this lognormal distribution. When lognormal_param1 is set to 1.63, the median latent period is 5.1 days (as is reported to be the case for COVID-19)

**days_past_symptom_patient_infective** is the number of days a patient is symptomatic before they are isolated and moved to a different ward.

**medic_medic_transmission_probability_factor** is a metric for how likely the SARS-COV-2 virus is to spread between medical staff. This metric should be set to a high number between 0 and 1 if the medical team members interact a lot with one another  and should be set to a low number between 0 and 1 if the medical team members maintain a lot of social distance from eachother while at the hospital ward.

**maximum_hours_per_week_nurse** is the maximum number of hours in a seven day period that a nurse is allowed to work. A nurse will not be assigned to work more than this many hours in a given 7 day period under any circumstances. Note that the average number of hours a nurse works per week is typically far below this threshold.

**n_attendings**, **n_residents**, and **n_nurses** are the number of attending physicians, residents and nurses on the team respectively.

**n_attentings_at_once**,**n_residents_at_once** and **n_nurses_at_once** are the number of team members that must be on duty at any given time in order to staff a fully functioning team.

## Running the function for varying choices of scheduling design parameters

Once the list of input parameters is set, the simulator can be run for varying choices of scheduling design.

To run the simulator, one can run the following command

sim_output <- COVID_sim9(length_rotation=6,length_rotation_n=3,nspd=2,staggering=FALSE,input_params=input_params,nit=10000)

Note that the arguments of the above function should be adjusted based on the scheduling design we are interested in exploring:

**length_rotation** is the length of physician rotations in days.

**length_rotation_n** is the length of nurse rotations in days (i.e the number of consecutive days in which each nurse is scheduled to work.) This input argument should be chosen keeping in mind that the nurses have a cap on how many hours they can work in a seven day span.

**nspd** is the number of nursing shifts per day. In our simulator this should ALWAYS be set to either 2 or 3. When it is set to 2, each nurse works 12 hour shifts. When it is set to 3, each nurse works 8 hour shifts.

**staggering** is an input argument that should be set to TRUE if we want our rotations to be staggered between various team members and should be set to FALSE if we want our rotations to be un-staggered (there are set days in which all the doctors rotate and set days in which all the nurses rotate (and if possible these are the same days))

**input_params** is the list of input parameters described in the previous section.

**nit** is the number of Monte Carlo simulations/expirements to run. If nit is too large, the runtime will be too long. If nit is too small then our error bars for the estimated probability of team failure will be too large.

If we would like to compare the schedules in which nurses work 8 hour shifts with schedules in which nurses work 12 hours shifts we can run the following to lines of code and compare the outputs.

sim_output8 <- COVID_sim9(length_rotation=3,length_rotation_n=3,nspd=3,staggering=FALSE,input_params=input_params,nit=10000)

sim_output12 <- COVID_sim9(length_rotation=3,length_rotation_n=3,nspd=2,staggering=FALSE,input_params=input_params,nit=10000)


## COVID_sim9 Function Outputs

COVID_sim9 returns a list with the following named components: failures,ac,rc,nc,af,rf,nf

**failures** is a logical vector with length equal to the number of Monte Carlo iterations which indicates which simulations had team failures and which simulations did not. The proportion of this vector that is true will give a point estimate for the probability of team failure under the inputted parameter choices and scheduling design.

**ac**, **rc** and **nc** are vectors with length equal to the number of Monte Carlo iterations that track the number of attendings, residents and nurses respectively who were infected during the course of each simulation.

**af**, **rf** and **nf** are logical vectors with length equal to the number of Monte Carlo iterations that track whether a shortage of attendings, residents or nurses respectively was responsible for team failure. All of these three logical vecotors have FALSE entries in rows corresponding to simulations in which the team didn't fail.


