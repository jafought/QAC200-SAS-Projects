/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Friday, January 09, 2015     TIME: 2:38:07 PM
PROJECT: FoughtJ_SAS_project_Jan 9 2015
PROJECT PATH: P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp
---------------------------------------- */

/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\jfought\Assignments' ;
/* Library assignment for Local.MYDATA */
Libname MYDATA V9 'P:\QAC\qac200\students\jfought\Assignments' ;


/* Conditionally delete set of tables or views, if they exists          */
/* If the member does not exist, then no action is performed   */
%macro _eg_conditional_dropds /parmbuff;
	
   	%local num;
   	%local stepneeded;
   	%local stepstarted;
   	%local dsname;
	%local name;

   	%let num=1;
	/* flags to determine whether a PROC SQL step is needed */
	/* or even started yet                                  */
	%let stepneeded=0;
	%let stepstarted=0;
   	%let dsname= %qscan(&syspbuff,&num,',()');
	%do %while(&dsname ne);	
		%let name = %sysfunc(left(&dsname));
		%if %qsysfunc(exist(&name)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;

			%end;
				drop table &name;
		%end;

		%if %sysfunc(exist(&name,view)) %then %do;
			%let stepneeded=1;
			%if (&stepstarted eq 0) %then %do;
				proc sql;
				%let stepstarted=1;
			%end;
				drop view &name;
		%end;
		%let num=%eval(&num+1);
      	%let dsname=%qscan(&syspbuff,&num,',()');
	%end;
	%if &stepstarted %then %do;
		quit;
	%end;
%mend _eg_conditional_dropds;


/* Build where clauses from stored process parameters */

%macro _eg_WhereParam( COLUMN, PARM, OPERATOR, TYPE=S, MATCHALL=_ALL_VALUES_, MATCHALL_CLAUSE=1, MAX= , IS_EXPLICIT=0);

  %local q1 q2 sq1 sq2;
  %local isEmpty;
  %local isEqual isNotEqual;
  %local isIn isNotIn;
  %local isString;
  %local isBetween;

  %let isEqual = ("%QUPCASE(&OPERATOR)" = "EQ" OR "&OPERATOR" = "=");
  %let isNotEqual = ("%QUPCASE(&OPERATOR)" = "NE" OR "&OPERATOR" = "<>");
  %let isIn = ("%QUPCASE(&OPERATOR)" = "IN");
  %let isNotIn = ("%QUPCASE(&OPERATOR)" = "NOT IN");
  %let isString = (%QUPCASE(&TYPE) eq S or %QUPCASE(&TYPE) eq STRING );
  %if &isString %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%");
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq D or %QUPCASE(&TYPE) eq DATE %then 
  %do;
    %let q1=%str(%");
    %let q2=%str(%"d);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq T or %QUPCASE(&TYPE) eq TIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"t);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else %if %QUPCASE(&TYPE) eq DT or %QUPCASE(&TYPE) eq DATETIME %then
  %do;
    %let q1=%str(%");
    %let q2=%str(%"dt);
	%let sq1=%str(%'); 
    %let sq2=%str(%'); 
  %end;
  %else
  %do;
    %let q1=;
    %let q2=;
	%let sq1=;
    %let sq2=;
  %end;
  
  %if "&PARM" = "" %then %let PARM=&COLUMN;

  %let isBetween = ("%QUPCASE(&OPERATOR)"="BETWEEN" or "%QUPCASE(&OPERATOR)"="NOT BETWEEN");

  %if "&MAX" = "" %then %do;
    %let MAX = &parm._MAX;
    %if &isBetween %then %let PARM = &parm._MIN;
  %end;

  %if not %symexist(&PARM) or (&isBetween and not %symexist(&MAX)) %then %do;
    %if &IS_EXPLICIT=0 %then %do;
		not &MATCHALL_CLAUSE
	%end;
	%else %do;
	    not 1=1
	%end;
  %end;
  %else %if "%qupcase(&&&PARM)" = "%qupcase(&MATCHALL)" %then %do;
    %if &IS_EXPLICIT=0 %then %do;
	    &MATCHALL_CLAUSE
	%end;
	%else %do;
	    1=1
	%end;	
  %end;
  %else %if (not %symexist(&PARM._count)) or &isBetween %then %do;
    %let isEmpty = ("&&&PARM" = "");
    %if (&isEqual AND &isEmpty AND &isString) %then
       &COLUMN is null;
    %else %if (&isNotEqual AND &isEmpty AND &isString) %then
       &COLUMN is not null;
    %else %do;
	   %if &IS_EXPLICIT=0 %then %do;
           &COLUMN &OPERATOR %unquote(&q1)&&&PARM%unquote(&q2)
	   %end;
	   %else %do;
	       &COLUMN &OPERATOR %unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2))
	   %end;
       %if &isBetween %then 
          AND %unquote(&q1)&&&MAX%unquote(&q2);
    %end;
  %end;
  %else 
  %do;
	%local emptyList;
  	%let emptyList = %symexist(&PARM._count);
  	%if &emptyList %then %let emptyList = &&&PARM._count = 0;
	%if (&emptyList) %then
	%do;
		%if (&isNotin) %then
		   1;
		%else
			0;
	%end;
	%else %if (&&&PARM._count = 1) %then 
    %do;
      %let isEmpty = ("&&&PARM" = "");
      %if (&isIn AND &isEmpty AND &isString) %then
        &COLUMN is null;
      %else %if (&isNotin AND &isEmpty AND &isString) %then
        &COLUMN is not null;
      %else %do;
	    %if &IS_EXPLICIT=0 %then %do;
            &COLUMN &OPERATOR (%unquote(&q1)&&&PARM%unquote(&q2))
	    %end;
		%else %do;
		    &COLUMN &OPERATOR (%unquote(%nrstr(&sq1))&&&PARM%unquote(%nrstr(&sq2)))
		%end;
	  %end;
    %end;
    %else 
    %do;
       %local addIsNull addIsNotNull addComma;
       %let addIsNull = %eval(0);
       %let addIsNotNull = %eval(0);
       %let addComma = %eval(0);
       (&COLUMN &OPERATOR ( 
       %do i=1 %to &&&PARM._count; 
          %let isEmpty = ("&&&PARM&i" = "");
          %if (&isString AND &isEmpty AND (&isIn OR &isNotIn)) %then
          %do;
             %if (&isIn) %then %let addIsNull = 1;
             %else %let addIsNotNull = 1;
          %end;
          %else
          %do;		     
            %if &addComma %then %do;,%end;
			%if &IS_EXPLICIT=0 %then %do;
                %unquote(&q1)&&&PARM&i%unquote(&q2) 
			%end;
			%else %do;
			    %unquote(%nrstr(&sq1))&&&PARM&i%unquote(%nrstr(&sq2)) 
			%end;
            %let addComma = %eval(1);
          %end;
       %end;) 
       %if &addIsNull %then OR &COLUMN is null;
       %else %if &addIsNotNull %then AND &COLUMN is not null;
       %do;)
       %end;
    %end;
  %end;
%mend;

/* ---------------------------------- */
/* MACRO: enterpriseguide             */
/* PURPOSE: define a macro variable   */
/*   that contains the file system    */
/*   path of the WORK library on the  */
/*   server.  Note that different     */
/*   logic is needed depending on the */
/*   server type.                     */
/* ---------------------------------- */
%macro enterpriseguide;
%global sasworklocation;
%local tempdsn unique_dsn path;

%if &sysscp=OS %then %do; /* MVS Server */
	%if %sysfunc(getoption(filesystem))=MVS %then %do;
        /* By default, physical file name will be considered a classic MVS data set. */
	    /* Construct dsn that will be unique for each concurrent session under a particular account: */
		filename egtemp '&egtemp' disp=(new,delete); /* create a temporary data set */
 		%let tempdsn=%sysfunc(pathname(egtemp)); /* get dsn */
		filename egtemp clear; /* get rid of data set - we only wanted its name */
		%let unique_dsn=".EGTEMP.%substr(&tempdsn, 1, 16).PDSE"; 
		filename egtmpdir &unique_dsn
			disp=(new,delete,delete) space=(cyl,(5,5,50))
			dsorg=po dsntype=library recfm=vb
			lrecl=8000 blksize=8004 ;
		options fileext=ignore ;
	%end; 
 	%else %do; 
        /* 
		By default, physical file name will be considered an HFS 
		(hierarchical file system) file. 
		*/
		%if "%sysfunc(getoption(filetempdir))"="" %then %do;
			filename egtmpdir '/tmp';
		%end;
		%else %do;
			filename egtmpdir "%sysfunc(getoption(filetempdir))";
		%end;
	%end; 
	%let path=%sysfunc(pathname(egtmpdir));
    %let sasworklocation=%sysfunc(quote(&path));  
%end; /* MVS Server */
%else %do;
	%let sasworklocation = "%sysfunc(getoption(work))/";
%end;
%if &sysscp=VMS_AXP %then %do; /* Alpha VMS server */
	%let sasworklocation = "%sysfunc(getoption(work))";                         
%end;
%if &sysscp=CMS %then %do; 
	%let path = %sysfunc(getoption(work));                         
	%let sasworklocation = "%substr(&path, %index(&path,%str( )))";
%end;
%mend enterpriseguide;

%enterpriseguide

/* save the current settings of XPIXELS and YPIXELS */
/* so that they can be restored later               */
%macro _sas_pushchartsize(new_xsize, new_ysize);
	%global _savedxpixels _savedypixels;
	options nonotes;
	proc sql noprint;
	select setting into :_savedxpixels
	from sashelp.vgopt
	where optname eq "XPIXELS";
	select setting into :_savedypixels
	from sashelp.vgopt
	where optname eq "YPIXELS";
	quit;
	options notes;
	GOPTIONS XPIXELS=&new_xsize YPIXELS=&new_ysize;
%mend;

/* restore the previous values for XPIXELS and YPIXELS */
%macro _sas_popchartsize;
	%if %symexist(_savedxpixels) %then %do;
		GOPTIONS XPIXELS=&_savedxpixels YPIXELS=&_savedypixels;
		%symdel _savedxpixels / nowarn;
		%symdel _savedypixels / nowarn;
	%end;
%mend;

ODS PROCTITLE;
OPTIONS DEV=ACTIVEX;
GOPTIONS XPIXELS=0 YPIXELS=0;
FILENAME EGSRX TEMP;
ODS tagsets.sasreport13(ID=EGSRX) FILE=EGSRX
    STYLE=HtmlBlue
    STYLESHEET=(URL="file:///C:/Program%20Files/SASHome/SASEnterpriseGuide/6.1/Styles/HtmlBlue.css")
    NOGTITLE
    NOGFOOTNOTE
    GPATH=&sasworklocation
    ENCODING=UTF8
    options(rolap="on")
;

/*   START OF NODE: Assign Project Library (MYDATA)   */
%LET _CLIENTTASKLABEL='Assign Project Library (MYDATA)';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\jfought\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "\\Client\P$\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:36:59 PM
   By task: Data Set Attributes

   Input Data: \\Client\P$\QAC\qac200\Data\MEPS\meps_fullyr_2012.sas7bdat
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA."%STR(FULLYR_2012 SUBSET)"n);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=ECLIB000.meps_fullyr_2012 OUT=WORK.SUCOUT1;

RUN;

DATA MYDATA.'FULLYR_2012 SUBSET'n(LABEL="Contents Details for meps_fullyr_2012");
   SET WORK.SUCOUT1;
RUN;

PROC DELETE DATA=WORK.SUCOUT1;
RUN;

%LET _LINESIZE=%SYSFUNC(GETOPTION(LINESIZE));

PROC SQL;
CREATE VIEW WORK.SCVIEW AS 
	SELECT DISTINCT memname LABEL="Table Name", 
			memlabel LABEL="Label", 
			memtype LABEL="Type", 
			crdate LABEL="Date Created", 
			modate LABEL="Date Modified", 
			nobs LABEL="Number of Obs.", 
			charset LABEL="Char. Set", 
			protect LABEL="Password Protected", 
			typemem LABEL="Data Set Type" FROM MYDATA.'FULLYR_2012 SUBSET'n
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MYDATA.'FULLYR_2012 SUBSET'n OUT=MYDATA.'FULLYR_2012 SUBSET'n;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MYDATA.'FULLYR_2012 SUBSET'n
		WHERE memname='MEPS_FULLYR_2012';
QUIT;

PROC REPORT DATA=WORK.SCTABLE NOWINDOWS; 
   FORMAT TYPE _EG_VARTYPE.; 
   DEFINE LABEL / DISPLAY WIDTH=&_LINESIZE; 
   LABEL NAME="Name" LABEL="Label" TYPE="Type" LENGTH="Length" INFORMAT="Informat" FORMAT="Format"; 
   BY memname NOTSORTED;  
   COLUMN name varnum type format label length;  
 QUIT;  

PROC SQL;
	DROP TABLE WORK.SCTABLE;
	DROP VIEW WORK.SCVIEW;
QUIT;

PROC CATALOG CATALOG=WORK.FORMATS;
   DELETE _EG_VARTYPE / ENTRYTYPE=FORMAT;
RUN;
OPTIONS BYLINE;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Subset Results   */
%LET _CLIENTTASKLABEL='Subset Results';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.FULLYR_2012_Filter_Results);

PROC SQL;
   CREATE TABLE MYDATA.FULLYR_2012_Filter_Results(label="FULLYR_2012_Assignment2 subset") AS 
   SELECT t1.DUPERSID, 
          t1.REGION12, 
          t1.SEX, 
          t1.AGE12X, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.SAQWT12F, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.OBCHIR12, 
          t1.DVORTH12, 
          t1.BLDRAGED, 
          t1.BRAIAGED, 
          t1.BRSTAGED, 
          t1.CERVAGED, 
          t1.COLOAGED, 
          t1.CHDAGED, 
          t1.LEUKAGED, 
          t1.LUNGAGED, 
          t1.LYMPAGED, 
          t1.MELAAGED, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.PRSTAGED, 
          t1.SKNMAGED, 
          t1.SKDKAGED, 
          t1.STRKAGED, 
          t1.THRTAGED, 
          t1.THYRAGED, 
          t1.OBVMCD12, 
          t1.OBVMCR12, 
          t1.OBVOTH12, 
          t1.OBVOPR12, 
          t1.OBVOPU12, 
          t1.OBVPRV12, 
          t1.OBVPTR12, 
          t1.OBVSLF12, 
          t1.OBVWCP12, 
          t1.DVTMCD12, 
          t1.DVTMCR12, 
          t1.DVTOTH12, 
          t1.DVTOPR12, 
          t1.DVTOPU12, 
          t1.DVTSTL12, 
          t1.DVTOFD12, 
          t1.DVTPRV12, 
          t1.DVTPTR12, 
          t1.DVTSLF12, 
          t1.DVTTRI12, 
          t1.DVTVA12, 
          t1.DVTWCP12, 
          t1.OBVSTL12, 
          t1.OBVOFD12, 
          t1.OBVTRI12, 
          t1.OBVVA12, 
          t1.BLDRREMS, 
          t1.BRAIREMS, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CASKINDK, 
          t1.CANCERDX, 
          t1.AMCMCD12, 
          t1.AMCMCR12, 
          t1.AMCOTH12, 
          t1.AMCOPR12, 
          t1.AMCOPU12, 
          t1.AMCSTL12, 
          t1.AMCOFD12, 
          t1.AMCPRV12, 
          t1.AMCSLF12, 
          t1.AMCVA12, 
          t1.AMCPTR12, 
          t1.AMCTRI12, 
          t1.AMCWCP12, 
          t1.AMCTCH12, 
          t1.OBCMCD12, 
          t1.OBCMCR12, 
          t1.OBCSTL12, 
          t1.OBCOFD12, 
          t1.OBCPRV12, 
          t1.OBCPTR12, 
          t1.OBCSLF12, 
          t1.OBCTRI12, 
          t1.OBCVA12, 
          t1.OBCWCP12, 
          t1.OBCOTH12, 
          t1.OBCOPR12, 
          t1.OBCOPU12, 
          t1.DNTINS12, 
          t1.BENDIF31, 
          t1.BENDIF53, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.RCHDIF31, 
          t1.RCHDIF53, 
          t1.STNDIF31, 
          t1.STNDIF53, 
          t1.FNGRDF31, 
          t1.FNGRDF53, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.MILDIF31, 
          t1.MILDIF53, 
          t1.STPDIF31, 
          t1.STPDIF53, 
          t1.BRSTEX53, 
          t1.MAMOGR53, 
          t1.JTPAIN31, 
          t1.JTPAIN53, 
          t1.LEUKREMS, 
          t1.LUNGREMS, 
          t1.LYMPREMS, 
          t1.MELAREMS, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.PRSTREMS, 
          t1.PHQ242, 
          t1.K6SUM42, 
          t1.SFFLAG42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SKNMREMS, 
          t1.SKDKREMS, 
          t1.THRTREMS, 
          t1.THYRREMS
      FROM EC100005.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS Program Code   */
%LET _CLIENTTASKLABEL='SAS Program Code';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _SASPROGRAMFILE='\\Client\P$\QAC\qac200\students\jfought\Assignments\SAS Program Code\SAS Program Code.sas';

GOPTIONS ACCESSIBLE;

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Thursday, January 08, 2015 at 11:11:47 AM
   By task: Data Set Attributes1

   Input Data: Local:MYDATA.FULLYR_2012_FILTER_RESULTS
   Server:  Local
   ------------------------------------------------------------------- */

TITLE "Data Set Attributes for Subset ;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.FULLYR_2012_FILTER_RESULTS;

RUN;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;
%LET _SASPROGRAMFILE=;


/*   START OF NODE: One-Way Frequencies For MRI and X rays for adults in 2012   */
%LET _CLIENTTASKLABEL='One-Way Frequencies For MRI and X rays for adults in 2012';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:01 PM
   By task: One-Way Frequencies For MRI and X rays for adults in 2012

   Input Data: Local:MYDATA.FULLYR_2012_FILTER_RESULTS
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.FULLYR_2012_FILTER_RESULTS
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.REGION12, T.SEX, T.AGE12X, T.RACETHX, T.MARRY12X, T.SAQWT12F, T.ADAPPT42, T.ADCAPE42, T.ADCLIM42, T.ADCMPD42, T.ADCMPM42, T.ADCMPY42, T.ADDAYA42, T.ADDOWN42, T.ADDPRS42, T.ADDRBP42, T.ADEFRT42, T.ADEGMC42, T.ADEXPL42, T.ADEZUN42
		     , T.ADFFRM42, T.ADFHLP42, T.ADGENH42, T.ADHECR42, T.ADHOPE42, T.ADILCR42, T.ADILWW42, T.ADINSA42, T.ADINSB42, T.ADINST42, T.ADINTR42, T.ADLANG42, T.ADLIST42, T.ADMALS42, T.ADMWLM42, T.ADNDCR42, T.ADNERV42, T.ADNRGY42
		     , T.ADNSMK42, T.ADOVER42, T.ADPAIN42, T.ADPALS42, T.ADPRTM42, T.ADPRX42, T.ADPWLM42, T.ADRESP42, T.ADREST42, T.ADRISK42, T.ADRTCR42, T.ADRTWW42, T.ADSAD42, T.ADSMOK42, T.ADSOCA42, T.ADSPEC42, T.ADSPRF42, T.ADTLHW42, T.ADWRTH42
		     , T.DVTOT12, T.ERTOT12, T.DVGEN12, T.OBCHIR12, T.DVORTH12, T.BLDRAGED, T.BRAIAGED, T.BRSTAGED, T.CERVAGED, T.COLOAGED, T.CHDAGED, T.LEUKAGED, T.LUNGAGED, T.LYMPAGED, T.MELAAGED, T.OTHRAGED, T.OHRTAGED, T.PRSTAGED, T.SKNMAGED
		     , T.SKDKAGED, T.STRKAGED, T.THRTAGED, T.THYRAGED, T.OBVMCD12, T.OBVMCR12, T.OBVOTH12, T.OBVOPR12, T.OBVOPU12, T.OBVPRV12, T.OBVPTR12, T.OBVSLF12, T.OBVWCP12, T.DVTMCD12, T.DVTMCR12, T.DVTOTH12, T.DVTOPR12, T.DVTOPU12
		     , T.DVTSTL12, T.DVTOFD12, T.DVTPRV12, T.DVTPTR12, T.DVTSLF12, T.DVTTRI12, T.DVTVA12, T.DVTWCP12, T.OBVSTL12, T.OBVOFD12, T.OBVTRI12, T.OBVVA12, T.BLDRREMS, T.BRAIREMS, T.BRSTREMS, T.CABLADDR, T.CABRAIN, T.CABREAST, T.CACERVIX
		     , T.CACOLON, T.CALEUKEM, T.CALUNG, T.CALYMPH, T.CAMELANO, T.CAOTHER, T.CAPROSTA, T.CASKINNM, T.CATHROAT, T.CATHYROD, T.CASKINDK, T.CANCERDX, T.AMCMCD12, T.AMCMCR12, T.AMCOTH12, T.AMCOPR12, T.AMCOPU12, T.AMCSTL12, T.AMCOFD12
		     , T.AMCPRV12, T.AMCSLF12, T.AMCVA12, T.AMCPTR12, T.AMCTRI12, T.AMCWCP12, T.AMCTCH12, T.OBCMCD12, T.OBCMCR12, T.OBCSTL12, T.OBCOFD12, T.OBCPRV12, T.OBCPTR12, T.OBCSLF12, T.OBCTRI12, T.OBCVA12, T.OBCWCP12, T.OBCOTH12, T.OBCOPR12
		     , T.OBCOPU12, T.DNTINS12, T.BENDIF31, T.BENDIF53, T.LFTDIF31, T.LFTDIF53, T.RCHDIF31, T.RCHDIF53, T.STNDIF31, T.STNDIF53, T.FNGRDF31, T.FNGRDF53, T.WLKDIF31, T.WLKDIF53, T.MILDIF31, T.MILDIF53, T.STPDIF31, T.STPDIF53, T.BRSTEX53
		     , T.MAMOGR53, T.JTPAIN31, T.JTPAIN53, T.LEUKREMS, T.LUNGREMS, T.LYMPREMS, T.MELAREMS, T.ACTDTY31, T.ACTDTY42, T.ACTDTY53, T.PRSTREMS, T.PHQ242, T.K6SUM42, T.SFFLAG42, T.MCS42, T.PCS42, T.SKNMREMS, T.SKDKREMS, T.THRTREMS
		     , T.THYRREMS
	FROM MYDATA.FULLYR_2012_FILTER_RESULTS(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results for MRI and X-ray visits for adults 18 and older in 2012";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by John Fought";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES REGION12 /  SCORES=TABLE;
	TABLES SEX /  SCORES=TABLE;
	TABLES AGE12X /  SCORES=TABLE;
	TABLES RACETHX /  SCORES=TABLE;
	TABLES MARRY12X /  SCORES=TABLE;
	TABLES SAQWT12F /  SCORES=TABLE;
	TABLES ADAPPT42 /  SCORES=TABLE;
	TABLES ADCAPE42 /  SCORES=TABLE;
	TABLES ADCLIM42 /  SCORES=TABLE;
	TABLES ADCMPD42 /  SCORES=TABLE;
	TABLES ADCMPM42 /  SCORES=TABLE;
	TABLES ADCMPY42 /  SCORES=TABLE;
	TABLES ADDAYA42 /  SCORES=TABLE;
	TABLES ADDOWN42 /  SCORES=TABLE;
	TABLES ADDPRS42 /  SCORES=TABLE;
	TABLES ADDRBP42 /  SCORES=TABLE;
	TABLES ADEFRT42 /  SCORES=TABLE;
	TABLES ADEGMC42 /  SCORES=TABLE;
	TABLES ADEXPL42 /  SCORES=TABLE;
	TABLES ADEZUN42 /  SCORES=TABLE;
	TABLES ADFFRM42 /  SCORES=TABLE;
	TABLES ADFHLP42 /  SCORES=TABLE;
	TABLES ADGENH42 /  SCORES=TABLE;
	TABLES ADHECR42 /  SCORES=TABLE;
	TABLES ADHOPE42 /  SCORES=TABLE;
	TABLES ADILCR42 /  SCORES=TABLE;
	TABLES ADILWW42 /  SCORES=TABLE;
	TABLES ADINSA42 /  SCORES=TABLE;
	TABLES ADINSB42 /  SCORES=TABLE;
	TABLES ADINST42 /  SCORES=TABLE;
	TABLES ADINTR42 /  SCORES=TABLE;
	TABLES ADLANG42 /  SCORES=TABLE;
	TABLES ADLIST42 /  SCORES=TABLE;
	TABLES ADMALS42 /  SCORES=TABLE;
	TABLES ADMWLM42 /  SCORES=TABLE;
	TABLES ADNDCR42 /  SCORES=TABLE;
	TABLES ADNERV42 /  SCORES=TABLE;
	TABLES ADNRGY42 /  SCORES=TABLE;
	TABLES ADNSMK42 /  SCORES=TABLE;
	TABLES ADOVER42 /  SCORES=TABLE;
	TABLES ADPAIN42 /  SCORES=TABLE;
	TABLES ADPALS42 /  SCORES=TABLE;
	TABLES ADPRTM42 /  SCORES=TABLE;
	TABLES ADPRX42 /  SCORES=TABLE;
	TABLES ADPWLM42 /  SCORES=TABLE;
	TABLES ADRESP42 /  SCORES=TABLE;
	TABLES ADREST42 /  SCORES=TABLE;
	TABLES ADRISK42 /  SCORES=TABLE;
	TABLES ADRTCR42 /  SCORES=TABLE;
	TABLES ADRTWW42 /  SCORES=TABLE;
	TABLES ADSAD42 /  SCORES=TABLE;
	TABLES ADSMOK42 /  SCORES=TABLE;
	TABLES ADSOCA42 /  SCORES=TABLE;
	TABLES ADSPEC42 /  SCORES=TABLE;
	TABLES ADSPRF42 /  SCORES=TABLE;
	TABLES ADTLHW42 /  SCORES=TABLE;
	TABLES ADWRTH42 /  SCORES=TABLE;
	TABLES DVTOT12 /  SCORES=TABLE;
	TABLES ERTOT12 /  SCORES=TABLE;
	TABLES DVGEN12 /  SCORES=TABLE;
	TABLES OBCHIR12 /  SCORES=TABLE;
	TABLES DVORTH12 /  SCORES=TABLE;
	TABLES BLDRAGED /  SCORES=TABLE;
	TABLES BRAIAGED /  SCORES=TABLE;
	TABLES BRSTAGED /  SCORES=TABLE;
	TABLES CERVAGED /  SCORES=TABLE;
	TABLES COLOAGED /  SCORES=TABLE;
	TABLES CHDAGED /  SCORES=TABLE;
	TABLES LEUKAGED /  SCORES=TABLE;
	TABLES LUNGAGED /  SCORES=TABLE;
	TABLES LYMPAGED /  SCORES=TABLE;
	TABLES MELAAGED /  SCORES=TABLE;
	TABLES OTHRAGED /  SCORES=TABLE;
	TABLES OHRTAGED /  SCORES=TABLE;
	TABLES PRSTAGED /  SCORES=TABLE;
	TABLES SKNMAGED /  SCORES=TABLE;
	TABLES SKDKAGED /  SCORES=TABLE;
	TABLES STRKAGED /  SCORES=TABLE;
	TABLES THRTAGED /  SCORES=TABLE;
	TABLES THYRAGED /  SCORES=TABLE;
	TABLES OBVMCD12 /  SCORES=TABLE;
	TABLES OBVMCR12 /  SCORES=TABLE;
	TABLES OBVOTH12 /  SCORES=TABLE;
	TABLES OBVOPR12 /  SCORES=TABLE;
	TABLES OBVOPU12 /  SCORES=TABLE;
	TABLES OBVPRV12 /  SCORES=TABLE;
	TABLES OBVPTR12 /  SCORES=TABLE;
	TABLES OBVSLF12 /  SCORES=TABLE;
	TABLES OBVWCP12 /  SCORES=TABLE;
	TABLES DVTMCD12 /  SCORES=TABLE;
	TABLES DVTMCR12 /  SCORES=TABLE;
	TABLES DVTOTH12 /  SCORES=TABLE;
	TABLES DVTOPR12 /  SCORES=TABLE;
	TABLES DVTOPU12 /  SCORES=TABLE;
	TABLES DVTSTL12 /  SCORES=TABLE;
	TABLES DVTOFD12 /  SCORES=TABLE;
	TABLES DVTPRV12 /  SCORES=TABLE;
	TABLES DVTPTR12 /  SCORES=TABLE;
	TABLES DVTSLF12 /  SCORES=TABLE;
	TABLES DVTTRI12 /  SCORES=TABLE;
	TABLES DVTVA12 /  SCORES=TABLE;
	TABLES DVTWCP12 /  SCORES=TABLE;
	TABLES OBVSTL12 /  SCORES=TABLE;
	TABLES OBVOFD12 /  SCORES=TABLE;
	TABLES OBVTRI12 /  SCORES=TABLE;
	TABLES OBVVA12 /  SCORES=TABLE;
	TABLES BLDRREMS /  SCORES=TABLE;
	TABLES BRAIREMS /  SCORES=TABLE;
	TABLES BRSTREMS /  SCORES=TABLE;
	TABLES CABLADDR /  SCORES=TABLE;
	TABLES CABRAIN /  SCORES=TABLE;
	TABLES CABREAST /  SCORES=TABLE;
	TABLES CACERVIX /  SCORES=TABLE;
	TABLES CACOLON /  SCORES=TABLE;
	TABLES CALEUKEM /  SCORES=TABLE;
	TABLES CALUNG /  SCORES=TABLE;
	TABLES CALYMPH /  SCORES=TABLE;
	TABLES CAMELANO /  SCORES=TABLE;
	TABLES CAOTHER /  SCORES=TABLE;
	TABLES CAPROSTA /  SCORES=TABLE;
	TABLES CASKINNM /  SCORES=TABLE;
	TABLES CATHROAT /  SCORES=TABLE;
	TABLES CATHYROD /  SCORES=TABLE;
	TABLES CASKINDK /  SCORES=TABLE;
	TABLES CANCERDX /  SCORES=TABLE;
	TABLES AMCMCD12 /  SCORES=TABLE;
	TABLES AMCMCR12 /  SCORES=TABLE;
	TABLES AMCOTH12 /  SCORES=TABLE;
	TABLES AMCOPR12 /  SCORES=TABLE;
	TABLES AMCOPU12 /  SCORES=TABLE;
	TABLES AMCSTL12 /  SCORES=TABLE;
	TABLES AMCOFD12 /  SCORES=TABLE;
	TABLES AMCPRV12 /  SCORES=TABLE;
	TABLES AMCSLF12 /  SCORES=TABLE;
	TABLES AMCVA12 /  SCORES=TABLE;
	TABLES AMCPTR12 /  SCORES=TABLE;
	TABLES AMCTRI12 /  SCORES=TABLE;
	TABLES AMCWCP12 /  SCORES=TABLE;
	TABLES AMCTCH12 /  SCORES=TABLE;
	TABLES OBCMCD12 /  SCORES=TABLE;
	TABLES OBCMCR12 /  SCORES=TABLE;
	TABLES OBCSTL12 /  SCORES=TABLE;
	TABLES OBCOFD12 /  SCORES=TABLE;
	TABLES OBCPRV12 /  SCORES=TABLE;
	TABLES OBCPTR12 /  SCORES=TABLE;
	TABLES OBCSLF12 /  SCORES=TABLE;
	TABLES OBCTRI12 /  SCORES=TABLE;
	TABLES OBCVA12 /  SCORES=TABLE;
	TABLES OBCWCP12 /  SCORES=TABLE;
	TABLES OBCOTH12 /  SCORES=TABLE;
	TABLES OBCOPR12 /  SCORES=TABLE;
	TABLES OBCOPU12 /  SCORES=TABLE;
	TABLES DNTINS12 /  SCORES=TABLE;
	TABLES BENDIF31 /  SCORES=TABLE;
	TABLES BENDIF53 /  SCORES=TABLE;
	TABLES LFTDIF31 /  SCORES=TABLE;
	TABLES LFTDIF53 /  SCORES=TABLE;
	TABLES RCHDIF31 /  SCORES=TABLE;
	TABLES RCHDIF53 /  SCORES=TABLE;
	TABLES STNDIF31 /  SCORES=TABLE;
	TABLES STNDIF53 /  SCORES=TABLE;
	TABLES FNGRDF31 /  SCORES=TABLE;
	TABLES FNGRDF53 /  SCORES=TABLE;
	TABLES WLKDIF31 /  SCORES=TABLE;
	TABLES WLKDIF53 /  SCORES=TABLE;
	TABLES MILDIF31 /  SCORES=TABLE;
	TABLES MILDIF53 /  SCORES=TABLE;
	TABLES STPDIF31 /  SCORES=TABLE;
	TABLES STPDIF53 /  SCORES=TABLE;
	TABLES BRSTEX53 /  SCORES=TABLE;
	TABLES MAMOGR53 /  SCORES=TABLE;
	TABLES JTPAIN31 /  SCORES=TABLE;
	TABLES JTPAIN53 /  SCORES=TABLE;
	TABLES LEUKREMS /  SCORES=TABLE;
	TABLES LUNGREMS /  SCORES=TABLE;
	TABLES LYMPREMS /  SCORES=TABLE;
	TABLES MELAREMS /  SCORES=TABLE;
	TABLES ACTDTY31 /  SCORES=TABLE;
	TABLES ACTDTY42 /  SCORES=TABLE;
	TABLES ACTDTY53 /  SCORES=TABLE;
	TABLES PRSTREMS /  SCORES=TABLE;
	TABLES PHQ242 /  SCORES=TABLE;
	TABLES K6SUM42 /  SCORES=TABLE;
	TABLES SFFLAG42 /  SCORES=TABLE;
	TABLES MCS42 /  SCORES=TABLE;
	TABLES PCS42 /  SCORES=TABLE;
	TABLES SKNMREMS /  SCORES=TABLE;
	TABLES SKDKREMS /  SCORES=TABLE;
	TABLES THRTREMS /  SCORES=TABLE;
	TABLES THYRREMS /  SCORES=TABLE;
RUN;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORT);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Recoded Variables   */
%LET _CLIENTTASKLABEL='Recoded Variables';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_FULLYR_2012);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_FULLYR_2012(label="QUERY_FOR_FULLYR_2012") AS 
   SELECT t1.DUPERSID, 
          t1.REGION12, 
          t1.SEX, 
          t1.AGE12X, 
          t1.RACETHX, 
          t1.MARRY12X, 
          t1.SAQWT12F, 
          t1.ADAPPT42, 
          t1.ADCAPE42, 
          t1.ADCLIM42, 
          t1.ADCMPD42, 
          t1.ADCMPM42, 
          t1.ADCMPY42, 
          t1.ADDAYA42, 
          t1.ADDOWN42, 
          t1.ADDPRS42, 
          t1.ADDRBP42, 
          t1.ADEFRT42, 
          t1.ADEGMC42, 
          t1.ADEXPL42, 
          t1.ADEZUN42, 
          t1.ADFFRM42, 
          t1.ADFHLP42, 
          t1.ADGENH42, 
          t1.ADHECR42, 
          t1.ADHOPE42, 
          t1.ADILCR42, 
          t1.ADILWW42, 
          t1.ADINSA42, 
          t1.ADINSB42, 
          t1.ADINST42, 
          t1.ADINTR42, 
          t1.ADLANG42, 
          t1.ADLIST42, 
          t1.ADMALS42, 
          t1.ADMWLM42, 
          t1.ADNDCR42, 
          t1.ADNERV42, 
          t1.ADNRGY42, 
          t1.ADNSMK42, 
          t1.ADOVER42, 
          t1.ADPAIN42, 
          t1.ADPALS42, 
          t1.ADPRTM42, 
          t1.ADPRX42, 
          t1.ADPWLM42, 
          t1.ADRESP42, 
          t1.ADREST42, 
          t1.ADRISK42, 
          t1.ADRTCR42, 
          t1.ADRTWW42, 
          t1.ADSAD42, 
          t1.ADSMOK42, 
          t1.ADSOCA42, 
          t1.ADSPEC42, 
          t1.ADSPRF42, 
          t1.ADTLHW42, 
          t1.ADWRTH42, 
          t1.DVTOT12, 
          t1.ERTOT12, 
          t1.DVGEN12, 
          t1.OBCHIR12, 
          t1.DVORTH12, 
          t1.BLDRAGED, 
          t1.BRAIAGED, 
          t1.BRSTAGED, 
          t1.CERVAGED, 
          t1.COLOAGED, 
          t1.CHDAGED, 
          t1.LEUKAGED, 
          t1.LUNGAGED, 
          t1.LYMPAGED, 
          t1.MELAAGED, 
          t1.OTHRAGED, 
          t1.OHRTAGED, 
          t1.PRSTAGED, 
          t1.SKNMAGED, 
          t1.SKDKAGED, 
          t1.STRKAGED, 
          t1.THRTAGED, 
          t1.THYRAGED, 
          t1.OBVMCD12, 
          t1.OBVMCR12, 
          t1.OBVOTH12, 
          t1.OBVOPR12, 
          t1.OBVOPU12, 
          t1.OBVPRV12, 
          t1.OBVPTR12, 
          t1.OBVSLF12, 
          t1.OBVWCP12, 
          t1.DVTMCD12, 
          t1.DVTMCR12, 
          t1.DVTOTH12, 
          t1.DVTOPR12, 
          t1.DVTOPU12, 
          t1.DVTSTL12, 
          t1.DVTOFD12, 
          t1.DVTPRV12, 
          t1.DVTPTR12, 
          t1.DVTSLF12, 
          t1.DVTTRI12, 
          t1.DVTVA12, 
          t1.DVTWCP12, 
          t1.OBVSTL12, 
          t1.OBVOFD12, 
          t1.OBVTRI12, 
          t1.OBVVA12, 
          t1.BLDRREMS, 
          t1.BRAIREMS, 
          t1.BRSTREMS, 
          t1.CABLADDR, 
          t1.CABRAIN, 
          t1.CABREAST, 
          t1.CACERVIX, 
          t1.CACOLON, 
          t1.CALEUKEM, 
          t1.CALUNG, 
          t1.CALYMPH, 
          t1.CAMELANO, 
          t1.CAOTHER, 
          t1.CAPROSTA, 
          t1.CASKINNM, 
          t1.CATHROAT, 
          t1.CATHYROD, 
          t1.CASKINDK, 
          t1.CANCERDX, 
          t1.AMCMCD12, 
          t1.AMCMCR12, 
          t1.AMCOTH12, 
          t1.AMCOPR12, 
          t1.AMCOPU12, 
          t1.AMCSTL12, 
          t1.AMCOFD12, 
          t1.AMCPRV12, 
          t1.AMCSLF12, 
          t1.AMCVA12, 
          t1.AMCPTR12, 
          t1.AMCTRI12, 
          t1.AMCWCP12, 
          t1.AMCTCH12, 
          t1.OBCMCD12, 
          t1.OBCMCR12, 
          t1.OBCSTL12, 
          t1.OBCOFD12, 
          t1.OBCPRV12, 
          t1.OBCPTR12, 
          t1.OBCSLF12, 
          t1.OBCTRI12, 
          t1.OBCVA12, 
          t1.OBCWCP12, 
          t1.OBCOTH12, 
          t1.OBCOPR12, 
          t1.OBCOPU12, 
          t1.DNTINS12, 
          t1.BENDIF31, 
          t1.BENDIF53, 
          t1.LFTDIF31, 
          t1.LFTDIF53, 
          t1.RCHDIF31, 
          t1.RCHDIF53, 
          t1.STNDIF31, 
          t1.STNDIF53, 
          t1.FNGRDF31, 
          t1.FNGRDF53, 
          t1.WLKDIF31, 
          t1.WLKDIF53, 
          t1.MILDIF31, 
          t1.MILDIF53, 
          t1.STPDIF31, 
          t1.STPDIF53, 
          t1.BRSTEX53, 
          t1.MAMOGR53, 
          t1.JTPAIN31, 
          t1.JTPAIN53, 
          t1.LEUKREMS, 
          t1.LUNGREMS, 
          t1.LYMPREMS, 
          t1.MELAREMS, 
          t1.ACTDTY31, 
          t1.ACTDTY42, 
          t1.ACTDTY53, 
          t1.PRSTREMS, 
          t1.PHQ242, 
          t1.K6SUM42, 
          t1.SFFLAG42, 
          t1.MCS42, 
          t1.PCS42, 
          t1.SKNMREMS, 
          t1.SKDKREMS, 
          t1.THRTREMS, 
          t1.THYRREMS, 
          /* ADGENH42_RECODED */
            (CASE 
               WHEN -1 = t1.ADGENH42 THEN .
               WHEN -8 = t1.ADGENH42 THEN .
               WHEN -9 = t1.ADGENH42 THEN .
               ELSE t1.ADGENH42
            END) LABEL="Health rating recoded" AS ADGENH42_RECODED, 
          /* ADDAYA42_RECODED */
            (CASE 
               WHEN -1 = t1.ADDAYA42 THEN .
               WHEN -9 = t1.ADDAYA42 THEN .
               ELSE t1.ADDAYA42
            END) LABEL="ADDAYA42 Recoded" AS ADDAYA42_RECODED, 
          /* ADCLIM42_RECODED */
            (CASE 
               WHEN -1 = t1.ADCLIM42 THEN .
               WHEN -8 = t1.ADCLIM42 THEN .
               WHEN -9 = t1.ADCLIM42 THEN .
               ELSE t1.ADCLIM42
            END) LABEL="health limits climbing stairs" AS ADCLIM42_RECODED, 
          /* ADPALS42_RECODED */
            (CASE 
               WHEN -1 = t1.ADPALS42 THEN .
               WHEN -9 = t1.ADPALS42 THEN .
               ELSE t1.ADPALS42
            END) LABEL="accmp less bc phy prbs" AS ADPALS42_RECODED, 
          /* ADPWLM42_RECODED */
            (CASE 
               WHEN -1 = t1.ADPWLM42 THEN .
               WHEN -9 = t1.ADPWLM42 THEN .
               ELSE t1.ADPWLM42
            END) LABEL="word limt bc phy probs" AS ADPWLM42_RECODED, 
          /* ADMALS42_RECODED */
            (CASE 
               WHEN -1 = t1.ADMALS42 THEN .
               WHEN -9 = t1.ADMALS42 THEN .
               ELSE t1.ADMALS42
            END) LABEL="accmp less bc mnt prbs" AS ADMALS42_RECODED, 
          /* ADMWLM42_RECODED */
            (CASE 
               WHEN -1 = t1.ADMWLM42 THEN .
               WHEN -7 = t1.ADMWLM42 THEN .
               WHEN -8 = t1.ADMWLM42 THEN .
               WHEN -9 = t1.ADMWLM42 THEN .
               ELSE t1.ADMWLM42
            END) LABEL="work lmt bc phy probs" AS ADMWLM42_RECODED, 
          /* ADPAIN421_RECODED */
            (CASE 
               WHEN -1 = t1.ADPAIN42 THEN .
               WHEN -9 = t1.ADPAIN42 THEN .
               ELSE t1.ADPAIN42
            END) LABEL="pain limits normal work" AS ADPAIN421_RECODED, 
          /* ADCAPE42_RECODED */
            (CASE 
               WHEN -1 = t1.ADCAPE42 THEN .
               WHEN -8 = t1.ADCAPE42 THEN .
               WHEN -9 = t1.ADCAPE42 THEN .
               ELSE t1.ADCAPE42
            END) LABEL="felt calm or peaceful" AS ADCAPE42_RECODED, 
          /* ADNRGY42_RECODED */
            (CASE 
               WHEN -1 = t1.ADNRGY42 THEN .
               WHEN -9 = t1.ADNRGY42 THEN .
               ELSE t1.ADNRGY42
            END) LABEL="had a lot of energy" AS ADNRGY42_RECODED, 
          /* ADDOWN42_RECODED */
            (CASE 
               WHEN -1 = t1.ADDOWN42 THEN .
               WHEN -8 = t1.ADDOWN42 THEN .
               WHEN -9 = t1.ADDOWN42 THEN .
               ELSE t1.ADDOWN42
            END) LABEL="felt downhearted or depr" AS ADDOWN42_RECODED, 
          /* ADSOCA42_RECODED */
            (CASE 
               WHEN -1 = t1.ADSOCA42 THEN .
               WHEN -9 = t1.ADSOCA42 THEN .
               ELSE t1.ADSOCA42
            END) LABEL="hlth stopped soc activ" AS ADSOCA42_RECODED, 
          /* ADAPPT42_RECODED */
            (CASE 
               WHEN -1 = t1.ADAPPT42 THEN .
               WHEN -8 = t1.ADAPPT42 THEN .
               WHEN -9 = t1.ADAPPT42 THEN .
            END) LABEL="No of med visits to off for care" AS ADAPPT42_RECODED, 
          /* ADDPRS42_RECODED */
            (CASE 
               WHEN -1 = t1.ADDPRS42 THEN .
               WHEN -7 = t1.ADDPRS42 THEN .
               WHEN -8 = t1.ADDPRS42 THEN .
               WHEN -9 = t1.ADDPRS42 THEN .
               ELSE t1.ADDPRS42
            END) LABEL="felt down or depressed or hopeless" AS ADDPRS42_RECODED, 
          /* ADDRBP42_RECODED */
            (CASE 
               WHEN -1 = t1.ADDRBP42 THEN .
               WHEN -8 = t1.ADDRBP42 THEN .
               WHEN -9 = t1.ADDRBP42 THEN .
               ELSE t1.ADDRBP42
            END) LABEL="Dr checked blood pressure" AS ADDRBP42_RECODED, 
          /* ADEFERT42_RECODED */
            (CASE 
               WHEN -1 = t1.ADEFRT42 THEN .
               WHEN -7 = t1.ADEFRT42 THEN .
               WHEN -8 = t1.ADEFRT42 THEN .
               WHEN -9 = t1.ADEFRT42 THEN .
               ELSE t1.ADEFRT42
            END) LABEL="how oftn everything an effort" AS ADEFERT42_RECODED, 
          /* ADEGMC42_RECODED */
            (CASE 
               WHEN -1 = t1.ADEGMC42 THEN .
               WHEN -9 = t1.ADEGMC42 THEN .
               ELSE t1.ADEGMC42
            END) LABEL="Easy getting needed med care" AS ADEGMC42_RECODED, 
          /* ADEXPL42_RECODED */
            (CASE 
               WHEN -1 = t1.ADEXPL42 THEN .
               WHEN -9 = t1.ADEXPL42 THEN .
               ELSE t1.ADEXPL42
            END) LABEL="Doc explained so understood" AS ADEXPL42_RECODED, 
          /* ADEZUN42_RECODED */
            (CASE 
               WHEN -1 = t1.ADEZUN42 THEN .
               WHEN -9 = t1.ADEZUN42 THEN .
               ELSE t1.ADEZUN42
            END) LABEL="Dr given instr EZ understood" AS ADEZUN42_RECODED, 
          /* ADFHLP42_RECODED */
            (CASE 
               WHEN -1 = t1.ADFHLP42 THEN .
               WHEN -8 = t1.ADFHLP42 THEN .
               WHEN -9 = t1.ADFHLP42 THEN .
               ELSE t1.ADFHLP42
            END) LABEL="offrd help filling out forms" AS ADFHLP42_RECODED, 
          /* ADHOPE42_RECODED */
            (CASE 
               WHEN -1 = t1.ADHOPE42 THEN .
               WHEN -7 = t1.ADHOPE42 THEN .
               WHEN -8 = t1.ADHOPE42 THEN .
               WHEN -9 = t1.ADHOPE42 THEN .
               ELSE t1.ADHOPE42
            END) LABEL="how often felt hopeless" AS ADHOPE42_RECODED, 
          /* ADREST42_RECODED */
            (CASE 
               WHEN -1 = t1.ADREST42 THEN .
               WHEN -7 = t1.ADREST42 THEN .
               WHEN -8 = t1.ADREST42 THEN .
               WHEN -9 = t1.ADREST42 THEN .
               ELSE t1.ADREST42
            END) LABEL="how often felt restless" AS ADREST42_RECODED, 
          /* BENDIF53_RECODED */
            (CASE 
               WHEN -1 = t1.BENDIF53 THEN .
               WHEN -7 = t1.BENDIF53 THEN .
               WHEN -8 = t1.BENDIF53 THEN .
               WHEN -9 = t1.BENDIF53 THEN .
               ELSE t1.BENDIF53
            END) LABEL="difficulty bending" AS BENDIF53_RECODED, 
          /* LFTDIF53_RECODED */
            (CASE 
               WHEN -1 = t1.LFTDIF53 THEN .
               WHEN -7 = t1.LFTDIF53 THEN .
               WHEN -8 = t1.LFTDIF53 THEN .
               WHEN -9 = t1.LFTDIF53 THEN .
               ELSE t1.LFTDIF53
            END) LABEL="difficulty lifting 10 pounds" AS LFTDIF53_RECODED, 
          /* RCHDIF53_RECODED */
            (CASE 
               WHEN -1 = t1.RCHDIF53 THEN .
               WHEN -7 = t1.RCHDIF53 THEN .
               WHEN -8 = t1.RCHDIF53 THEN .
               WHEN -9 = t1.RCHDIF53 THEN .
               ELSE t1.RCHDIF53
            END) LABEL="difficulty reaching overhead" AS RCHDIF53_RECODED, 
          /* STNDIF53_RECODED */
            (CASE 
               WHEN -1 = t1.STNDIF53 THEN .
               WHEN -7 = t1.STNDIF53 THEN .
               WHEN -8 = t1.STNDIF53 THEN .
               WHEN -9 = t1.STNDIF53 THEN .
               ELSE t1.STNDIF53
            END) LABEL="difficulty standing for 20 min" AS STNDIF53_RECODED, 
          /* WLKDIF53_RECODED */
            (CASE 
               WHEN -1 = t1.WLKDIF53 THEN .
               WHEN -7 = t1.WLKDIF53 THEN .
               WHEN -8 = t1.WLKDIF53 THEN .
               WHEN -9 = t1.WLKDIF53 THEN .
               ELSE t1.WLKDIF53
            END) LABEL="difficulty walking 3 blocks" AS WLKDIF53_RECODED, 
          /* BRSTEX53_RECODED */
            (CASE 
               WHEN -1 = t1.BRSTEX53 THEN .
               WHEN -7 = t1.BRSTEX53 THEN .
               WHEN -8 = t1.BRSTEX53 THEN .
               WHEN -9 = t1.BRSTEX53 THEN .
               ELSE t1.BRSTEX53
            END) LABEL="how long since last breast exam" AS BRSTEX53_RECODED, 
          /* JTPAIN53_RECODED */
            (CASE 
               WHEN -1 = t1.JTPAIN53 THEN .
               WHEN -7 = t1.JTPAIN53 THEN .
               WHEN -8 = t1.JTPAIN53 THEN .
               WHEN -9 = t1.JTPAIN53 THEN .
               ELSE t1.JTPAIN53
            END) LABEL="joint pain last 12 months" AS JTPAIN53_RECODED, 
          /* ACTDTY42_RECODED */
            (CASE 
               WHEN -1 = t1.ACTDTY42 THEN .
               WHEN -7 = t1.ACTDTY42 THEN .
               WHEN -8 = t1.ACTDTY42 THEN .
               WHEN -9 = t1.ACTDTY42 THEN .
               ELSE t1.ACTDTY42
            END) LABEL="military full time active duty" AS ACTDTY42_RECODED, 
          /* STPDIF53_RECODED */
            (CASE 
               WHEN -1 = t1.STPDIF53 THEN .
               WHEN -7 = t1.STPDIF53 THEN .
               WHEN -8 = t1.STPDIF53 THEN .
               WHEN -9 = t1.STPDIF53 THEN .
               ELSE t1.STPDIF53
            END) LABEL="difficulty walking up 10 steps" AS STPDIF53_RECODED, 
          /* MAMOGR53_RECODED */
            (CASE 
               WHEN -1 = t1.MAMOGR53 THEN .
               WHEN -7 = t1.MAMOGR53 THEN .
               WHEN -8 = t1.MAMOGR53 THEN .
               WHEN -9 = t1.MAMOGR53 THEN .
               ELSE t1.MAMOGR53
            END) LABEL="how lng snce lst mamogr" AS MAMOGR53_RECODED
      FROM MYDATA.FULLYR_2012_FILTER_RESULTS t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADGENH42   */
%LET _CLIENTTASKLABEL='ADGENH42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:01 PM
   By task: ADGENH42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADGENH42
	FROM MYDATA.QUERY_FOR_FULLYR_2012 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42 * Health_in_general_ADGENH42 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADDAYA42   */
%LET _CLIENTTASKLABEL='ADDAYA42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:01 PM
   By task: ADDAYA42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDAYA42
	FROM MYDATA.QUERY_FOR_FULLYR_2012 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDAYA42 * "Calculation Recoded"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADCLIM42   */
%LET _CLIENTTASKLABEL='ADCLIM42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: ADCLIM42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCLIM42, T.ADCLIM42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCLIM42 * ADCLIM42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADPALS42   */
%LET _CLIENTTASKLABEL='ADPALS42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: ADPALS42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPALS42, T.ADPALS42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012 as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPALS42 * ADPALS42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADPWLM42   */
%LET _CLIENTTASKLABEL='ADPWLM42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: ADPWLM42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPWLM42, T.ADPWLM42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPWLM42 * ADPWLM42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADMALS42   */
%LET _CLIENTTASKLABEL='ADMALS42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: ADMALS42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADMALS42, T.ADMALS42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMALS42 * ADMALS42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADMWLM42   */
%LET _CLIENTTASKLABEL='ADMWLM42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: ADMWLM42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADMWLM42, T.ADMWLM42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADMWLM42 * ADMWLM42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADPAIN42   */
%LET _CLIENTTASKLABEL='ADPAIN42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: ADPAIN42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADPAIN42, T.ADPAIN421_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADPAIN42 * ADPAIN421_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MAMOGR53   */
%LET _CLIENTTASKLABEL='MAMOGR53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:02 PM
   By task: MAMOGR53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MAMOGR53
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MAMOGR53 * MAMOGR531 /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADCAPE42   */
%LET _CLIENTTASKLABEL='ADCAPE42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: ADCAPE42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADCAPE42, T.ADCAPE42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADCAPE42 * ADCAPE42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: STPDIF53   */
%LET _CLIENTTASKLABEL='STPDIF53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: STPDIF53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.STPDIF53, T.STPDIF53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES STPDIF53 * STPDIF53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADNRGY42   */
%LET _CLIENTTASKLABEL='ADNRGY42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: ADNRGY42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADNRGY42, T.ADNRGY42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADNRGY42 * ADNRGY42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ACTDTY42   */
%LET _CLIENTTASKLABEL='ACTDTY42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: ACTDTY42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ACTDTY42, T.ACTDTY42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ACTDTY42 * ACTDTY42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADDOWN42   */
%LET _CLIENTTASKLABEL='ADDOWN42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: ADDOWN42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDOWN42, T.ADDOWN42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDOWN42 * ADDOWN42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADSOCA42   */
%LET _CLIENTTASKLABEL='ADSOCA42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: ADSOCA42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADSOCA42, T.ADSOCA42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADSOCA42 * ADSOCA42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: JTPAIN53   */
%LET _CLIENTTASKLABEL='JTPAIN53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:03 PM
   By task: JTPAIN53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.JTPAIN53, T.JTPAIN53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES JTPAIN53 * JTPAIN53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: BRSTEX42   */
%LET _CLIENTTASKLABEL='BRSTEX42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: BRSTEX42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.BRSTEX53, T.BRSTEX53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES BRSTEX53 * BRSTEX53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADAPPT42   */
%LET _CLIENTTASKLABEL='ADAPPT42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: ADAPPT42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADAPPT42, T.ADAPPT42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADAPPT42 * ADAPPT42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADDPRS42   */
%LET _CLIENTTASKLABEL='ADDPRS42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: ADDPRS42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDPRS42, T.ADDPRS42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDPRS42 * ADDPRS42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADDRPB42   */
%LET _CLIENTTASKLABEL='ADDRPB42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: ADDRPB42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDRBP42, T.ADDRBP42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADDRBP42 * ADDRBP42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADEFERT42   */
%LET _CLIENTTASKLABEL='ADEFERT42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: ADEFERT42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADEFRT42, T.ADEFERT42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEFRT42 * ADEFERT42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADEGMC42   */
%LET _CLIENTTASKLABEL='ADEGMC42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: ADEGMC42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADEGMC42, T.ADEGMC42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEGMC42 * ADEGMC42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADEXPL42   */
%LET _CLIENTTASKLABEL='ADEXPL42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:04 PM
   By task: ADEXPL42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADEXPL42, T.ADEXPL42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEXPL42 * ADEXPL42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADEZUN42   */
%LET _CLIENTTASKLABEL='ADEZUN42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: ADEZUN42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADEZUN42, T.ADEZUN42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADEZUN42 * ADEZUN42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADFHLP42   */
%LET _CLIENTTASKLABEL='ADFHLP42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: ADFHLP42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADFHLP42, T.ADFHLP42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADFHLP42 * ADFHLP42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: BENDIF53   */
%LET _CLIENTTASKLABEL='BENDIF53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: BENDIF53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.BENDIF53, T.BENDIF53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES BENDIF53 * BENDIF53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADREST42   */
%LET _CLIENTTASKLABEL='ADREST42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: ADREST42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADREST42, T.ADREST42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADREST42 * ADREST42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: ADHOPE42   */
%LET _CLIENTTASKLABEL='ADHOPE42';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: ADHOPE42

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADHOPE42, T.ADHOPE42_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADHOPE42 * ADHOPE42_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: LFTDIF53   */
%LET _CLIENTTASKLABEL='LFTDIF53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: LFTDIF53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.LFTDIF53, T.LFTDIF53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES LFTDIF53 * LFTDIF53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: WLKDIF53   */
%LET _CLIENTTASKLABEL='WLKDIF53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:05 PM
   By task: WLKDIF53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.WLKDIF53, T.WLKDIF53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES WLKDIF53 * WLKDIF53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: STNDIF53   */
%LET _CLIENTTASKLABEL='STNDIF53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:06 PM
   By task: STNDIF53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.STNDIF53, T.STNDIF53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES STNDIF53 * STNDIF53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: RCHDIF53   */
%LET _CLIENTTASKLABEL='RCHDIF53';
%LET _CLIENTPROJECTPATH='P:\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 9 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 9 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Friday, January 09, 2015 at 2:37:06 PM
   By task: RCHDIF53

   Input Data: Local:MYDATA.QUERY_FOR_FULLYR_2012
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_FULLYR_2012
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.RCHDIF53, T.RCHDIF53_RECODED
	FROM MYDATA.QUERY_FOR_FULLYR_2012(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES RCHDIF53 * RCHDIF53_RECODED /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
