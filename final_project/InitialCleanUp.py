
import sys
import csv
import time
import os

def convertData(origData):
	# Check if incoming string is empty
	if origData == "":
		return "", ""
	else:
		tempTime = time.strptime(origData, "%Y-%m-%d")
		year = time.strftime("%Y", tempTime)
		epochTime = time.mktime(tempTime)
	
		return year, epochTime

def stateFullToAbbrev(state):
	# Convert Abbreviation to full name
	abbreviation = ["bc", "mh", "vi","mp","gu", "pr","dc","al","ak","az","ar","ca","co","ct","de","fl","ga","hi","id","il","in","ia","ks","ky","la","me","md","ma","mi","mn","ms","mo","mt","ne","nv","nh","nj","nm","ny","nc","nd","oh","ok","or","pa","ri","sc","sd","tn","tx","ut","vt","va","wa","wv","wi","wy"]
	stateFull = ["british columbia","marshall islands","virgin islands","northern mariana islands","guam","puerto rico","district of columbia","alabama","alaska","arizona","arkansas","california","colorado","connecticut","delaware","florida","georgia","hawaii","idaho","illinois","indiana","iowa","kansas","kentucky","louisiana","maine","maryland","massachusetts","michigan","minnesota","mississippi","missouri","montana","nebraska","nevada","new hampshire","new jersey","new mexico","new york","north carolina","north dakota","ohio","oklahoma","oregon","pennsylvania","rhode island","south carolina","south dakota","tennessee","texas","utah","vermont","virginia","washington","west virginia","wisconsin","wyoming"]
	
	stateDict = dict(zip(stateFull,abbreviation))
	
	state = state.lower()
	
	# is the input an abbreviation?
	if state in stateDict:
		return stateDict[state]
	else: # no? return the value as it was
		return state
		
def main():
	print("Reading in CSV Data\n(Second argument should be the location to the CSV file)")
	
	csvFileName = sys.argv[1]
	fullFilePath = os.path.abspath(csvFileName)
	csvfilePath, csvFileName = os.path.split(fullFilePath)
	csvBaseFileName, csvExt = os.path.splitext(csvFileName)
	fullReadFileName = os.path.join(csvfilePath, csvBaseFileName + csvExt)
	fullWriteFileName = os.path.join(csvfilePath, csvBaseFileName + "_UPDATED" + csvExt)
	print("Reading:\t" + fullFilePath)

	fileObjRead = open(csvFileName, 'r', encoding='cp850')
	#fileObjRead = open(csvFileName, 'rb')
	fileReader = csv.DictReader(fileObjRead)
	
	listOfDateFields = ["case_received_date", "decision_date", "orig_file_date"]
	fieldsIncsv = fileReader.fieldnames.copy()
	for field in listOfDateFields:
		fieldsIncsv.append(field + "_YEAR")
		fieldsIncsv.append(field + "_EPOCH")

	# Remove second argument per list...
	listOfFieldsSame = [["country_of_citizenship", "country_of_citzenship"], ["case_no", "case_number"], ["wage_offer_from_9089", "wage_offered_from_9089"], ["wage_offer_unit_of_pay_9089","wage_offered_unit_of_pay_9089"]]
	for field in listOfFieldsSame:
		fieldsIncsv.remove(field[1])
		
	# list of fields that may have state as an abbreviation... Let's make them all
	# abbreviations...
	listOfStateFields = ["employer_state", "foreign_worker_info_state", "agent_state"]
		
	#listOfFieldsToRmv = ["ri_pvt_employment_firm_from","ji_live_in_dom_svc_contract","ji_live_in_domestic_service","ri_job_search_website_from","ri_job_search_website_to","job_info_alt_occ_job_title","job_info_combo_occupation","ri_1st_ad_newspaper_name","ri_local_ethnic_paper_from","ri_local_ethnic_paper_to","ri_posted_notice_at_worksite","us_economic_sector","job_info_alt_occ_num_months","ri_2nd_ad_newspaper_name","ri_2nd_ad_newspaper_or_journal","ri_campus_placement_from","ri_campus_placement_to","ri_coll_tch_basic_process","ri_coll_teach_pro_jnl","ri_coll_teach_select_date","ri_employee_referral_prog_from","ri_employee_referral_prog_to","ri_pvt_employment_firm_to","agent_state","job_info_alt_occ","ri_employer_web_post_from","ri_employer_web_post_to","foreign_worker_info_city","agent_city","agent_firm_name","naics_2007_us_code","naics_2007_us_title","naics_code","naics_title","naics_us_code","naics_us_code_2007","naics_us_title","naics_us_title_2007","recr_info_radio_tv_ad_from","recr_info_radio_tv_ad_to","employer_address_1","recr_info_sunday_newspaper","employer_address_2","schd_a_sheepherder","add_these_pw_job_title_9089","employer_phone","employer_phone_ext","pw_soc_code","pw_soc_title","pw_source_name_9089","pw_source_name_other_9089","pw_track_num","pw_unit_of_pay_9089","recr_info_job_fair_from","recr_info_job_fair_to","recr_info_on_campus_recr_from","recr_info_on_campus_recr_to","recr_info_pro_org_advert_from","recr_info_pro_org_advert_to","recr_info_prof_org_advert_from","recr_info_prof_org_advert_to","recr_info_second_ad_start","recr_info_swa_job_order_end","recr_info_swa_job_order_start","foreign_worker_info_inst","foreign_worker_info_training_comp","job_info_training","fw_info_training_comp","ji_foreign_worker_live_on_premises","job_info_training_field","rec_info_barg_rep_notified","recr_info_barg_rep_notified","orig_case_no","recr_info_professional_occ","job_info_training_num_months","preparer_info_title","recr_info_coll_univ_teacher","wage_offer_unit_of_pay_9089","employer_postal_code","ji_fw_live_on_premises","ri_layoff_in_past_six_months"]

		
	fileObjWrite = open(fullWriteFileName, 'w', encoding='cp850')
	#fileObjWrite = open(fullWriteFileName, 'wb')
	fileWriter = csv.DictWriter(fileObjWrite, fieldsIncsv)
	fileWriter.writeheader()
	
	print("Done reading file and now processing variables")
	
	for row in fileReader:
		# Get all fields with Time information we care about
		# and make new variables field1_YEAR and field1_EPOCH
		# append these to the row dictionary
		#row = rowMain.copy()
		newRow = {}
		
		for field in listOfDateFields:
			tempData = row[field] # get the current row's date from the current variable

			# Get Time since Epoch and only the Year
			tempYear, tempEpochTime = convertData(tempData)
			
			# Add these new variables back to the current row
			field_YEAR = field + "_YEAR"
			field_EPOCH = field + "_EPOCH"
			row[field_YEAR] = tempYear
			row[field_EPOCH] = tempEpochTime
			
		# Insert any other modifications we want here...
		for field in listOfFieldsSame:
			tempA = row[field[0]] # keep the name of Add
			tempB = row[field[1]]
			if tempA == "" and tempB == "":
				row[field[0]] = ""
			elif tempA == "":
				row[field[0]] = tempB
			else:
				row[field[0]] = tempA
			#del row[field[1]]
			

			# assuming no intersect...
			
		for field in listOfStateFields:
			row[field] = stateFullToAbbrev(row[field])
			
		for key in fieldsIncsv:				
			newRow[key] = row[key]
		
		# Write the current row back to th new file
		fileWriter.writerow(newRow)
		
	
		
	# Done writing all rows that we wanted... 
	fileObjWrite.close()
	
	
if __name__ == "__main__":
	main()