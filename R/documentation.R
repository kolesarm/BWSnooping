#' Snooping-adjusted critical values
#'
#' Data frame of precomputed snooping-adjusted critical values, using the
#' function \code{\link{DFSnoopingCV}}. The data frame is used by
#' \code{\link{SnoopingCV}} to look up the appropriate critical value
#' @format There are TODO rows and 9 columns:
#' \describe{
#'   \item{kernel}{kernel function}
#'   \item{order}{Order of local polynomial (0 for local constant)}
#'   \item{boundary}{Boundary or interior regression?}
#'   \item{t}{ratio of maximum to minimum bandwidth}
#'   \item{level}{confidence level}
#'   \item{onesided}{Critical value for one-sided CIs}
#'   \item{twosided}{Critical value for two-sided CIs}
#'   \item{ua.onesided}{Coverage of unadjusted one-sided CIs}
#'   \item{ua.onesided}{Coverage of unadjusted two-sided CIs}
#' }
#'
#' @source Computed by running
#' \code{snoopingcvs <- DFSnoopingCV(S=60000, T=10000, 1000)}
"snoopingcvs"

#' Right-heart catheterization data
#'
#' Dataset from Connors et al. (1996). This version labels and modifies the
#' variables so that they match those in Hirano and Imbens' analysis.
#'
#' @source \url{http://biostat.mc.vanderbilt.edu/wiki/Main/DataSets}
#' @format There are 5,735 observations on 54 variables. The treatment and
#'     outcome variables are
#' \describe{
#' \item{rhc}{Right Heart Catheterization (RHC, also called Swan-Ganz catheter)
#'          received within 24 hours after entering study.}
#' \item{survival}{Indicator for 30-day survival}
#' }
#' Socioeconomic variables:
#' \describe{
#' \item{age}{Age (years)}
#' \item{sex}{Female indicator}
#' \item{race}{White/Black/Other}
#' \item{edu}{Years of education}
#' \item{income}{Under $11k / $11-$25k / $25-$50k / > $50k}
#' \item{insurance}{No insurance / Private / Medicare / Medicaid / Private &
#'     Medicare / Medicare & Medicaid}
#' }
#' Disease categories:
#' \describe{
#' \item{cat1}{Primary disease category. ARF (acute respiratory failure) / COPD
#'           (chronic obsructive pulmonary disease) / CHF (congestive heart
#'           failure) / Cirrhosis / Coma (nontraumatic coma) / Colon Cancer
#'           (metastatic to the liver) / Lung Cancer (stage III or IV) / MOSF
#'           w/Malignancy (multi-organ system failure) / MOSF w/Sepsis}
#' \item{cat2}{Secondary disease category. Same categories as cat1, plus "None"}
#' \item{cancer}{None/Localized/Metastatic}
#' }
#' Categories of admission diagnosis:
#' \describe{
#' \item{ad_resp}{Respiratory}
#' \item{ad_card}{Cardiovascular}
#' \item{ad_neuro}{Neurological}
#' \item{ad_gastr}{Gastrointestinal}
#' \item{ad_renal}{Renal}
#' \item{ad_meta}{Metabolic}
#' \item{ad_hema}{Hematologic}
#' \item{ad_seps}{Sepsis}
#' \item{ad_trauma}{Trauma}
#' \item{ad_ortho}{Orthopedic}
#' }
#' Comorbidities:
#' \describe{
#' \item{hx_cardio}{Cardiovascular symptoms}
#' \item{hx_chf}{Congestive Heart Failure}
#' \item{hx_dement}{Dementia, Stroke or Cerebral Infarct, Parkinson's Disease}
#' \item{hx_psych}{Psychiatric History, Active Psychosis or Severe Depression}
#' \item{hx_cpd}{Chronic Pulmonary Disease, Severe Pulmonary Disease, Very Severe
#'             Pulmonary Disease}
#' \item{hx_renal}{Chronic Renal Disease, Chronic Hemodialysis or Peritoneal
#'    Dialysis}
#' \item{hx_liver}{Cirrhosis, Hepatic Failure}
#' \item{hx_gibleed}{Upper GI Bleeding.}
#' \item{hx_malig}{Solid Tumor, Metastatic Disease, Chronic Leukemia/Myeloma,
#'    AcuteLeukemia, Lymphoma}
#' \item{hx_immuno}{Immunosupperssion, Organ Transplant, HIV Positive, Diabetes}
##                Mellitus Without End Organ Damage, Diabetes Mellitus With End
##                Organ Damage, Connective Tissue Disease}
#' \item{hx_ami}{Definite Myocardial Infarction}
#' \item{hx_transfer}{Transfer (> 24 Hours) from Another Hospital}
#' }
#' Physiological status on Day 1:
#' \describe{
#' \item{dnr1}{Do Not Resuscitate (DNR) status}
#' \item{surv2md1}{SUPPORT model estimate of 2-month survival probability}
#' \item{aps1}{APACHE III score (Acute Physiology and Chronic Health Evaluation),
#'     ignoring Coma}
#' \item{scoma1}{SUPPORT Glasgow Coma Score}
#' \item{wtkilo1}{weight in kilograms}
#' \item{wt0}{wtkilo==0}
#' \item{temp1}{Temperature (Celcius)}
#' \item{meanbp1}{Mean Blood Pressure (mm Hg)}
#' \item{resp1}{Respiratory rate (breaths/min)}
#' \item{hrt1}{Heart rate (beats/min)}
#' \item{pafi1}{PaO_2/(0.01*FiO_2)}
#' \item{paco21}{PaCO_2  (partial pressure of arterial carbon dioxide)}
#' \item{ph1}{Serum pH (arterial)}
#' \item{wbc1}{White Blood Cell (WBC) count}
#' \item{hema1}{Hematocrit}
#' \item{sod1}{Serum sodium}
#' \item{pot1}{Serum potassium}
#' \item{crea1}{Serum creatinine}
#' \item{bili1}{Bilirubin}
#' \item{alb1}{Albumin}
#' }
#' Other variables: \describe{\item{das2d3pc}{Duke Activity Status Index (DASI)
#' 2 weeks prior ## "rhc"}}
"rhc"

#' National Supported Work (NWS) Demonstration data
#'
#' Dataset from Dehejia and Wahba (1999).
#' @source Rajeev Dehejia's website,
#'     \url{http://users.nber.org/~rdehejia/nswdata2.html}
#' @format Data frame with 2,675 observations on 12 variables:
#' \describe{
#' \item{treatment}{treatment indicator}
#' \item{age}{Age in years}
#' \item{education}{Years of schooling}
#' \item{black}{}
#' \item{hispanic}{}
#' \item{married}{}
#' \item{nodegree}{No high school degree}
#' \item{re74}{Earnings in 1974}
#' \item{re75}{Earnings in 1975}
#' \item{re78}{Earnings in 1978}
#' \item{ue74}{Unemployed (i.e. zero earnings) in 1974}
#' \item{ue74}{Unemployed (i.e. zero earnings) in 1974}
#' }
"nsw"
