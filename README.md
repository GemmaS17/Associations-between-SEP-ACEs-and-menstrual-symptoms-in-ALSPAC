This analysis included 4 main parts, each with multiple scripts (12 in total).
1. Associations between SEP and menstrual symptoms in ALSPAC G1, two scripts include:
-> G1_SEP_ToSymptoms.do (main stata do file)
-> G1_MI_IPW_Sensitivity_SEP.do (stata do file conducting an alternative MI approach used in a sensitivity analysis)
2. Associations between ACEs and menstrual symptoms in ALSPAC G1, six scripts include:
-> G1_ACEs_ToSymptoms.do (main stata do file), with mentions of 3 R scripts ran at various stages
-> Script_1_G1_ACEs.R (R script that recodes original ACE variables to binary)
-> Script_2_G1_ACEs.R (R script that calculates ACE constructs)
-> Script_3_G1_ACEs.R (R script that runs MI)
-> Alternative_Script3_Imp_PerOutcome.R (R script that conducts an alternative MI approach used in a sensitivity analysis)
-> G1_MI_IPW_Sensitivity_ACEs.do (stata do file that runs analysis using the alternative MI from above R script)
3. Associations between SEP and menstrual symptoms in ALSPAC G0, two scripts include:
-> G0_SEP_ToSymptoms.do (main stata do file)
-> G0_MI_IPW_Sensitivity_SEP.do (stata do file conducting an alternative MI approach used in a sensitivity analysis)
4. Associations between ACEs and menstrual symptoms in ALSPAC G0, two scripts include:
-> G0_ACEs_ToSymptoms.do (main stata do file)
-> G0_MI_IPW_Sensitivity_ACEs.do (stata do file conducting an alternative MI approach used in a sensitivity analysis)
