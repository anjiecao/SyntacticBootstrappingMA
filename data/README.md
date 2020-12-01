### Codebook for Syntactic Bootstrapping Meta-analysis Data 

[1] `raw/syntactic_bootstrapping_raw_data.csv` -

	* exp - original experiment number.
	* exp_recoded - experiment number in paper
	* one_3sub_label - identifies whether the single exemplar and 3-subordinate exemplar trial received the same label.
	* blocking - were the trials blocked by type or pseudo random?
	* order - in what order were the single exemplar and 3-subordinate exemplar trials presented?
	* timing - were training exemplars presented simultaneously (as in XT) or sequentially as in (SPSS)?
	* direct_replication_of - corresponding replication study
	* preregistered - was the study preregistered?

[2] `processed/syntactic_bootstrapping_tidy_data.csv` - output of `get_literature_ES.R`. Calculated effect sizes for the 7 prior experiments in the literature.

	* exp_recoded - experiment number in paper
	* n - number of participants
	* d - cohens d
	* d_var - variance on d
	* high/low - 95% CI

[3] `metalab/Metalab Challenge Data - Syntactic Bootstrapping - Sheet1.csv` - output of `analysis/munge_anonymize_data.R`. It is the data from all 12 experiments in the rawest form. The only processing that has been done on this data is binding files from all the experiments together, adding an experiment id column, and anonymizing the subject ids. Each row corresponds to one participant.

	* ApprovalTime/AutoApprovalTime/AssignmentId/HITId/Assignment/AssignmentStatus - misc turk variables
	* AcceptTime/SubmitTime - actual time of acceptance and completetion of the task
	* exp - original experiment number	
	* subids - anonymized subject id
	* trainBlock_T* - block name (varied depending on whether the experiment was blocked or pseudo-random; * = trial num)
	* trainPics_T* - list of training exemplars for that trial (* = trial num)
	* condition_T* - condition name (* = trial num)
	* category_T* -  target category of that trial (vegetables, animals, vehicles; * = trial num)
	* word_T* - novel word used on that trial (* = trial num)
	* selected_T* - list of testing exemplars selected for that trial (* = trial num)
	* enjoyment/asses/comments - post-task questions (Did you enjoy the task?/Were you confused?)
	* gender/age/language/education/ - post-task demographic questions

[4] `metalab/metalab_raw.csv` - output of `analysis/munge_anonymize_data.R`. This file contains all the data processed into long form so that each row corresponds to one trial. This is the file that is used in data analysis.

	

[5] `anonymized/no_dups_data_munged_A.csv` - output of `analysis/munge_anonymize_data_no_dups.R`. This is the same as all_data_munged_A.csv, except that duplicate participants have been removed.