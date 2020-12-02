### Codebook for Syntactic Bootstrapping Meta-analysis Data 

[1] `raw/syntactic_bootstrapping_raw_data.csv` - this file includes all the key statistics and coded moderators used in the meta-analysis. It is the output of `analysis/scripts/01_tidy_raw_ma_data.R`. 

	* coder: initials of the coders. AC = Anjie Cao; ML = Molly Lewis. 
	* unique_id: the unique id assigned to each individual paper. 
	* long_cite: APA format citation for the paper.
	* link: the link to the paper (if available). 
	* grammatical_class: the grammatical category of the novel word to be learned. 
	* paper_eligible: if the paper meets our inclusion criteria. 
	* short_cite: shortened version of the long cite. 
	* data_source: the sources of the key statistics (result text or table / measured from figure / through author contact)
	* expt_num: the index of the experiment as listed in the paper 
	* plot_label: unique identifier for individual condition
	* expt_condition: the name of the experiment condition as described in the paper 
	* dependent_measure: the measurement used in the paper (point_scene / looking_duration)
	* response_mode: metalab category (behavior if dependent_measure is point_scene / eye-tracking if the looking_duration is measured by eye tracker / looking if the looking_duration is hand coded)
	* test_type: whether the children are expected to identify from the visual stimuli 
	* se: standard error of the mean looking time, if available 
	* same_infant: the same string identifies infants that have been tested in multiple conditions. 
	* language: the native language and test language of the participants. 
	* mean_age: mean age of the participant, in days. 
	* productive_vocab_mean: mean productive vocabulary size as measured by short-form CDI. 
	* productive_vocab_median: median productive vocabulary size as measured by short-form CDI.
	* population_type: if the participants are typically developing children. 
	* sentence_structure: the structure of linguistic stimuli used in testing. If the sentence has two or more arguments, then it is a transitive sentence. Otherwise intransitive 
	* agent_argument_type: the type of words used in the the agent argument of the linguistic stimuli. If there is one occurence of pronoun, then it is coded as pronoun. Otherwise nouns.
	* patient_argument_type: the type of words used in the the patient argument of the linguistic stimuli. If there is one occurence of pronoun, then it is coded as pronoun. Otherwise nouns. For intransitive sentence, it is coded as INTRANSITIVE. 
	* verb_type: if the verbs to be learned are fake verbs or real verbs.  
	* stimuli_type: type of visual stimuli.
	* stimuli_modality: the type of media used to present the stimuli. 
	* stimuli_actor: the protagonists in the video stimuli. If the video's protagonists are human actors wearing animal suits, it is coded as non_person as well. 
	* transitive_event: the type of action shown in the transitive event. 
	* intransitive_event: the type of action shown in the transitive event.
	* presentation_type: the alignment between the onset of the linguistic stimuli and the visual stimuli.
	* character_identification: whether the testing procedure includes a phase where the children were introduced to the characters in the scenes.
	* practice_phase: whether the testing procedure includes a phase where the children were prompted to identify familiar actions.
	* test_mass_or_distributed: whether the testing procedure contains more than one train-test pair.
	* n_train_test_pair: the number of train-test pair.
	* n_test_trial: the number of trials in one train-test pair.
	* n_repetitions_sentence: Per novel verb, the number of repetitions of the linguistic stimuli the children were exposed to before tested.
	* n_repetitions_video: Per novel verb, the number of repetitions of the visual stimuli the children were exposed to before.
	* example_target_sentence: An example of the training sentence used in the condition.
	* test_question: An example of the testing sentence used in the condition.
	* n_1: sample size of the conidtion.
	* x_1: standardized mean looking time. 
	* x_2: chance level. 
	* x_2_raw: If the original condition includes a baseline measurement, then the standardized mean looking time. 
	* sd_1: standardized looking time standard deviation. 
	* sd_2: standardized looking time standard deviation, same as sd_1. 
	* sd_2_raw: standardized looking time standard deviation of the x_2_raw.
	* t: t statistics, if reported in the origianl paper. 
	* d: Cohen's d, if reported in the original paper. 
	
	
	

[2] `processed/syntactic_bootstrapping_tidy_data.csv` - output of `analysis/scripts/02_calculate_es.R`. This file includes all the columns in `raw/syntactic_bootstrapping_raw_data.csv` and the calculated Cohen's d and variance. For the details of calculating effect size, see [SI](https://rpubs.com/anjiecao/671474)

	* n - number of participants
	* d - cohens d
	* d_var - variance on d

[3] `metalab/SyntacticBootstrapping - Data.csv` - source data currently used in Metalab (December 2020). You can access the live version [here](https://docs.google.com/spreadsheets/d/1NQka41XhYMIrCk427mEWiqgNjQVKReKUo4d9e-s1rKs/edit?usp=sharing)


[4] `metalab/metalab_raw.csv` - output of the function `download_metalab_data` in `writeups/paper/scripts/metalab_plot_helper.R`. This function uses `metalabr` package and download data for mutual exclusivity, cross-situational learning, gaze-following, and sound symbolism. 
