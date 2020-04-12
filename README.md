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

**T** is the number of days the simulation runs for.

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

## Running the function for varying choices of scheduling design parameters

## Function Outputs
