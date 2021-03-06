/* ----------------------------------------
Code exported from SAS Enterprise Guide
DATE: Tuesday, January 13, 2015     TIME: 1:32:25 PM
PROJECT: FoughtJ_SAS_project_Jan 13 2015
PROJECT PATH: \\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp
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
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME MYDATA  "P:\QAC\qac200\students\jfought\Assignments" ;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
LIBNAME ECLIB000 "\\Client\P$\QAC\qac200\Data\MEPS";

/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:31 PM
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
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

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
          t1.THYRREMS, 
          t1.EDRECODE
      FROM EC100010.meps_fullyr_2012 t1
      WHERE t1.AGE12X >= 18;
QUIT;

GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: SAS Program Code   */
%LET _CLIENTTASKLABEL='SAS Program Code';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';
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
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:37 PM
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
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

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
            END) LABEL="how lng snce lst mamogr" AS MAMOGR53_RECODED, 
          /* MARRYX12_RECODED */
            (CASE 
               WHEN -7 = t1.MARRY12X THEN .
               WHEN -9 = t1.MARRY12X THEN .
               ELSE t1.MARRY12X
            END) LABEL="Marital status recoded" AS MARRYX12_RECODED, 
          /* EDRECODE_RECODED */
            (CASE 
               WHEN -7 = t1.EDRECODE THEN .
               WHEN -8 = t1.EDRECODE THEN .
               WHEN -9 = t1.EDRECODE THEN .
               ELSE t1.EDRECODE
            END) LABEL="Education Level recoded" AS EDRECODE_RECODED
      FROM MYDATA.FULLYR_2012_FILTER_RESULTS t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Query for Rescored SF-12   */
%LET _CLIENTTASKLABEL='Query for Rescored SF-12';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA."Output Data Set Rescored"n);

PROC SQL;
   CREATE TABLE MYDATA."Output Data Set Rescored"n(label="Output Data Set Rescored") AS 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          /* ADGENH42_RESCORED */
            (6-t1.ADGENH42_RECODED) LABEL="reversed rescored" AS ADGENH42_RESCORED, 
          /* ADPAIN42_RESCORED */
            (6-t1.ADPAIN421_RECODED) LABEL="reverse scored" AS ADPAIN42_RESCORED, 
          /* ADCAPE42_RESCORED */
            (6-t1.ADCAPE42_RECODED) LABEL="reverse scored" AS ADCAPE42_RESCORED, 
          /* ADNRGY42_RESCORED */
            (6-t1.ADNRGY42_RECODED) LABEL="reversed scored" AS ADNRGY42_RESCORED
      FROM MYDATA.QUERY_FOR_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Reverse Rescored Variables Analysis   */
%LET _CLIENTTASKLABEL='Reverse Rescored Variables Analysis';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:39 PM
   By task: Reverse Rescored Variables Analysis

   Input Data: Local:MYDATA.OUTPUT DATA SET RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.OUTPUT DATA SET RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADNRGY42_RECODED, T.ADCAPE42_RECODED, T.ADPAIN421_RECODED, T.ADGENH42_RECODED
	FROM MYDATA.'OUTPUT DATA SET RESCORED'n(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results for Rescored Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) by John Fought";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES ADGENH42_RECODED * "ADGENH42 RESCORED"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADCAPE42_RECODED * "ADCAPE42 RESCORED"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADPAIN421_RECODED * "ADPAIN42 RESCORED"n /
		NOROW
		NOCOL
		NOPERCENT
		MISSPRINT
		NOCUM
		SCORES=TABLE
		ALPHA=0.05;
	TABLES ADNRGY42_RECODED * "ADNRGY42 RESCORED"n /
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


/*   START OF NODE: Aggregate Rescored Data Set SF12   */
%LET _CLIENTTASKLABEL='Aggregate Rescored Data Set SF12';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_AGGREGATE_RESCORED);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_AGGREGATE_RESCORED(label="QUERY_FOR_AGGREGATE_RESCORED") AS 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          /* Aggregate_SUM_Rescored */
            
            (SUM(t1.ADGENH42_RESCORED,t1.ADPAIN42_RESCORED,t1.ADCAPE42_RESCORED,t1.ADNRGY42_RESCORED,t1.ADDAYA42_RECODED,t1.ADCLIM42_RECODED,t1.ADPALS42_RECODED,t1.ADPWLM42_RECODED,t1.ADMALS42_RECODED,t1.ADMWLM42_RECODED,t1.ADDOWN42_RECODED,t1.ADSOCA42_RECODED)) 
            LABEL="Sum of rescored data set" AS Aggregate_SUM_Rescored
      FROM MYDATA.'OUTPUT DATA SET RESCORED'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: First 50 Observations   */
%LET _CLIENTTASKLABEL='First 50 Observations';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:40 PM
   By task: First 50 Observations

   Input Data: Local:MYDATA.QUERY_FOR_AGGREGATE_RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_AGGREGATE_RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.ADDOWN42_RECODED, T.ADSOCA42_RECODED, T.ADCLIM42_RECODED, T.ADPALS42_RECODED, T.ADPWLM42_RECODED, T.ADMALS42_RECODED, T.ADMWLM42_RECODED, T.ADDAYA42_RECODED, T.ADGENH42_RESCORED, T.ADPAIN42_RESCORED, T.ADCAPE42_RESCORED
		     , T.ADNRGY42_RESCORED, T.Aggregate_SUM_Rescored
	FROM MYDATA.QUERY_FOR_AGGREGATE_RESCORED(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Aggregate health Score for First 50 Observations for SF-12 Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR ADDOWN42_RECODED ADSOCA42_RECODED ADCLIM42_RECODED ADPALS42_RECODED ADPWLM42_RECODED ADMALS42_RECODED ADMWLM42_RECODED ADDAYA42_RECODED ADGENH42_RESCORED ADPAIN42_RESCORED ADCAPE42_RESCORED ADNRGY42_RESCORED Aggregate_SUM_Rescored;
RUN;
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


/*   START OF NODE: Summary of Aggregate Data Set   */
%LET _CLIENTTASKLABEL='Summary of Aggregate Data Set';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:41 PM
   By task: Summary of Aggregate Data Set

   Input Data: Local:MYDATA.QUERY_FOR_AGGREGATE_RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_AGGREGATE_RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_SUM_Rescored
	FROM MYDATA.QUERY_FOR_AGGREGATE_RESCORED(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics";
TITLE2 "Results For Aggregate Sum of SF-12 Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE NONOBS 	
		Q1 
		MEDIAN 
		Q3	;
	VAR Aggregate_SUM_Rescored;

RUN;
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


/*   START OF NODE: Distribution Analysis for Aggregate SF-12 Variables   */
%LET _CLIENTTASKLABEL='Distribution Analysis for Aggregate SF-12 Variables';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:42 PM
   By task: Distribution Analysis for Aggregate SF-12 Variables

   Input Data: Local:MYDATA.QUERY_FOR_AGGREGATE_RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_AGGREGATE_RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_SUM_Rescored
	FROM MYDATA.QUERY_FOR_AGGREGATE_RESCORED(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Aggregate_SUM_Rescored";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR Aggregate_SUM_Rescored;
	HISTOGRAM   Aggregate_SUM_Rescored / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Aggregate SF12 Catagorical   */
%LET _CLIENTTASKLABEL='Aggregate SF12 Catagorical';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_AGGREGATE_Catagorical);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_AGGREGATE_Catagorical(label="QUERY_FOR_AGGREGATE_Catagorical") AS 
   SELECT t1.DUPERSID, 
          /* Aggregate_SF12_Catagorical */
            (CASE  
               WHEN t1.Aggregate_SUM_Rescored >= 2 and t1.Aggregate_SUM_Rescored < 41
               THEN 1
               WHEN t1.Aggregate_SUM_Rescored >= 41 and t1.Aggregate_SUM_Rescored < 48
               THEN 2
               WHEN t1.Aggregate_SUM_Rescored >= 48 and t1.Aggregate_SUM_Rescored < 52
               THEN 3
               ELSE 4
            END) LABEL="SF-12 Variables Catagorical by Quartiles" AS Aggregate_SF12_Catagorical, 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          t1.Aggregate_SUM_Rescored
      FROM MYDATA.QUERY_FOR_AGGREGATE_RESCORED t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Comparison table - Quantitative & Catagorical   */
%LET _CLIENTTASKLABEL='Comparison table - Quantitative & Catagorical';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:43 PM
   By task: Comparison table - Quantitative & Catagorical

   Input Data: Local:MYDATA.QUERY_FOR_AGGREGATE_CATAGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.QUERY_FOR_AGGREGATE_CATAGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_SUM_Rescored, T.Aggregate_SF12_Catagorical
	FROM MYDATA.QUERY_FOR_AGGREGATE_CATAGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results For Comparision between Aggregate Quantitative and Aggregate Catagorical Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES Aggregate_SUM_Rescored * Aggregate_SF12_Catagorical /
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


/*   START OF NODE: Query for INFULLYR   */
%LET _CLIENTTASKLABEL='Query for INFULLYR';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.INFULLYR_VARIABLE);

PROC SQL;
   CREATE TABLE MYDATA.INFULLYR_VARIABLE(label="INFULLYR_VARIABLE") AS 
   SELECT t1.DUPERSID, 
          t1.Aggregate_SF12_Catagorical, 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          t1.Aggregate_SUM_Rescored, 
          /* INFULLYR */
            (1) AS INFULLYR
      FROM MYDATA.QUERY_FOR_AGGREGATE_CATAGORICAL t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Movement Difficulty Rescored   */
%LET _CLIENTTASKLABEL='Movement Difficulty Rescored';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA."QUERY_FOR_Movement Rescored"n);

PROC SQL;
   CREATE TABLE MYDATA."QUERY_FOR_Movement Rescored"n(label="QUERY_FOR_Movement Rescored") AS 
   SELECT /* BENDIF53_RESCORED */
            (5-t1.BENDIF53_RECODED) LABEL="Trouble Bending Rescored" AS BENDIF53_RESCORED, 
          /* LFTDIF53_RESCORED */
            (5-t1.LFTDIF53_RECODED) LABEL="Trouble lifting 10 lbs Rescored" AS LFTDIF53_RESCORED, 
          /* RCHDIF53_RESCORED */
            (5-t1.RCHDIF53_RECODED) LABEL="Difficulty reaching overhead Rescored" AS RCHDIF53_RESCORED, 
          /* STNDIF53_RESCORED */
            (5-t1.STNDIF53_RECODED) LABEL="Difficulty Standing 20 min Rescored" AS STNDIF53_RESCORED
      FROM MYDATA.QUERY_FOR_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Aggregate Movement Difficulty Variable   */
%LET _CLIENTTASKLABEL='Aggregate Movement Difficulty Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.QUERY_FOR_MOVEMENT_RESCORED);

PROC SQL;
   CREATE TABLE MYDATA.QUERY_FOR_MOVEMENT_RESCORED(label="QUERY_FOR_MOVEMENT_RESCORED") AS 
   SELECT /* Aggregate_Movement_Variable */
            (SUM(t1.BENDIF53_RESCORED,t1.LFTDIF53_RESCORED,t1.RCHDIF53_RESCORED,t1.STNDIF53_RESCORED)) LABEL=
            "Sum of Movement difficulty Variables" AS Aggregate_Movement_Variable, 
          t1.BENDIF53_RESCORED, 
          t1.LFTDIF53_RESCORED, 
          t1.RCHDIF53_RESCORED, 
          t1.STNDIF53_RESCORED
      FROM MYDATA.'QUERY_FOR_MOVEMENT RESCORED'n t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: MARRY12X CATAGORICAL   */
%LET _CLIENTTASKLABEL='MARRY12X CATAGORICAL';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MARRY12X_CATAGORICAL);

PROC SQL;
   CREATE TABLE MYDATA.MARRY12X_CATAGORICAL(label="MARRY12X_CATAGORICAL") AS 
   SELECT /* MARRY12X_CATAGORICAL */
            (CASE  
               WHEN t1.MARRYX12_RECODED = 1
               THEN 1
               WHEN t1.MARRYX12_RECODED = 5
               THEN 2
               ELSE 3
            END) LABEL="Marital Status Catagories" AS MARRY12X_CATAGORICAL, 
          t1.MARRYX12_RECODED
      FROM MYDATA.QUERY_FOR_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Comparison of Catagorical MARRY12X Variable and Original   */
%LET _CLIENTTASKLABEL='Comparison of Catagorical MARRY12X Variable and Original';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:45 PM
   By task: Comparison of Catagorical MARRY12X Variable and Original

   Input Data: Local:MYDATA.MARRY12X_CATAGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MARRY12X_CATAGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.MARRY12X_CATAGORICAL, T.MARRYX12_RECODED
	FROM MYDATA.MARRY12X_CATAGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results For Comparison of MARRY12X Catagorical variable with original values";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES MARRYX12_RECODED * MARRY12X_CATAGORICAL /
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


/*   START OF NODE: EDRECODE CATAGORICAL   */
%LET _CLIENTTASKLABEL='EDRECODE CATAGORICAL';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.EDRECODE_CATAGORICAL);

PROC SQL;
   CREATE TABLE MYDATA.EDRECODE_CATAGORICAL(label="EDRECODE_CATAGORICAL") AS 
   SELECT /* EDRECODE_CATAGORIZED */
            (CASE  
               WHEN t1.EDRECODE_RECODED <= 12
               THEN 1
               WHEN t1.EDRECODE_RECODED = 13
               THEN 2
               WHEN t1.EDRECODE_RECODED = 14
               THEN 3
               WHEN t1.EDRECODE_RECODED = 15
               THEN 4
               ELSE 16
            END) LABEL="Education level Catagorized" AS EDRECODE_CATAGORIZED, 
          t1.EDRECODE_RECODED
      FROM MYDATA.QUERY_FOR_FULLYR_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Comparison of Catagorical EDRECODE and Original Variable   */
%LET _CLIENTTASKLABEL='Comparison of Catagorical EDRECODE and Original Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:46 PM
   By task: Comparison of Catagorical EDRECODE and Original Variable

   Input Data: Local:MYDATA.EDRECODE_CATAGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.EDRECODE_CATAGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.EDRECODE_RECODED, T.EDRECODE_CATAGORIZED
	FROM MYDATA.EDRECODE_CATAGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results For Comparison of EDRECODE Catagorized and Original Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES EDRECODE_RECODED * EDRECODE_CATAGORIZED /
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


/*   START OF NODE: First 50 Observations for Movement Difficulty   */
%LET _CLIENTTASKLABEL='First 50 Observations for Movement Difficulty';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 5:21:04 PM
   By task: First 50 Observations for Movement Difficulty

   Input Data: Local:WORK.QUERY_FOR_MOVEMENT_RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MOVEMENT_RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.BENDIF53_RESCORED, T.LFTDIF53_RESCORED, T.RCHDIF53_RESCORED, T.STNDIF53_RESCORED
	FROM WORK.QUERY_FOR_MOVEMENT_RESCORED(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing First 50 Observations for Movement Difficulty";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=50)
	OBS="Row number"
	LABEL
	;
	VAR BENDIF53_RESCORED LFTDIF53_RESCORED RCHDIF53_RESCORED STNDIF53_RESCORED;
RUN;
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


/*   START OF NODE: Summary For Aggregate Movment Difficulty Variable   */
%LET _CLIENTTASKLABEL='Summary For Aggregate Movment Difficulty Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 5:42:21 PM
   By task: Summary For Aggregate Movment Difficulty Variable

   Input Data: Local:WORK.QUERY_FOR_MOVEMENT_RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MOVEMENT_RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_Movement_Variable
	FROM WORK.QUERY_FOR_MOVEMENT_RESCORED(FIRSTOBS=1 ) as T
;
QUIT;
/* -------------------------------------------------------------------
   Run the Means Procedure
   ------------------------------------------------------------------- */
TITLE;
TITLE1 "Summary Statistics For Aggregate Movement Difficulty Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC MEANS DATA=WORK.SORTTempTableSorted
	FW=12
	PRINTALLTYPES
	CHARTYPE
	QMETHOD=OS
	VARDEF=DF 	
		MEAN 
		STD 
		MODE NONOBS 	
		Q1 
		MEDIAN 
		Q3	;
	VAR Aggregate_Movement_Variable;

RUN;
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


/*   START OF NODE: Distribution Analysis For Aggregate Movement Difficulty Variable   */
%LET _CLIENTTASKLABEL='Distribution Analysis For Aggregate Movement Difficulty Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;

%put ERROR: The task refers to variables that are not in the input data source.
Please review the variable assignments.
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Monday, January 12, 2015 at 5:21:05 PM
   By task: Distribution Analysis For Aggregate Movement Difficulty Variable

   Input Data: Local:WORK.QUERY_FOR_MOVEMENT_RESCORED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:WORK.QUERY_FOR_MOVEMENT_RESCORED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT *
	FROM WORK.QUERY_FOR_MOVEMENT_RESCORED(FIRSTOBS=1 )
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: Aggregate Movement Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	HISTOGRAM   / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Movement Difficulty Catagorical Variable   */
%LET _CLIENTTASKLABEL='Movement Difficulty Catagorical Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%put ERROR: Unable to get SAS code. Unable to open input data;


GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Comparison Table for Quantitative and Catagorical Movement Difficulty Variables   */
%LET _CLIENTTASKLABEL='Comparison Table for Quantitative and Catagorical Movement Difficulty Variables';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:47 PM
   By task: Comparison Table for Quantitative and Catagorical Movement Difficulty Variables

   Input Data: Local:MYDATA.MOVEMENT_DIFFICULTY_CATAGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MOVEMENT_DIFFICULTY_CATAGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.Aggregate_Movement_Variable, T.Movement_Difficulty_Catagorical
	FROM MYDATA.MOVEMENT_DIFFICULTY_CATAGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results Comparing the Quantitative and Catagorical Variables for Movement Difficulty";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES Aggregate_Movement_Variable * Movement_Difficulty_Catagorical /
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


/*   START OF NODE: Query Builder1   */
LIBNAME EC100012 "\\Client\P$\QAC\qac200\Data\MEPS";


%LET _CLIENTTASKLABEL='Query Builder1';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.IN_ER_VARIABLE);

PROC SQL;
   CREATE TABLE MYDATA.IN_ER_VARIABLE(label="IN_ER_VARIABLE") AS 
   SELECT t1.DUID, 
          t1.PID, 
          t1.DUPERSID, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          /* IN_ER */
            (1) AS IN_ER
      FROM EC100012.meps_er_2012 t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Data Set Merger   */
%LET _CLIENTTASKLABEL='Data Set Merger';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MERGER_FOR_FULLYR_INER);

PROC SQL;
   CREATE TABLE MYDATA.MERGER_FOR_FULLYR_INER(label="MERGER_FOR_FULLYR_INER") AS 
   SELECT t1.DUPERSID, 
          t1.Aggregate_SF12_Catagorical, 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          t1.Aggregate_SUM_Rescored, 
          t1.INFULLYR, 
          t2.DUID, 
          t2.PID, 
          t2.DUPERSID AS DUPERSID1, 
          t2.EVNTIDX, 
          t2.EVENTRN, 
          t2.ERHEVIDX, 
          t2.FFEEIDX, 
          t2.PANEL, 
          t2.MPCDATA, 
          t2.ERDATEYR, 
          t2.ERDATEMM, 
          t2.ERDATEDD, 
          t2.SEEDOC, 
          t2.VSTCTGRY, 
          t2.VSTRELCN, 
          t2.LABTEST, 
          t2.SONOGRAM, 
          t2.XRAYS, 
          t2.MAMMOG, 
          t2.MRI, 
          t2.EKG, 
          t2.EEG, 
          t2.RCVVAC, 
          t2.ANESTH, 
          t2.THRTSWAB, 
          t2.OTHSVCE, 
          t2.SURGPROC, 
          t2.MEDPRESC, 
          t2.ERICD1X, 
          t2.ERICD2X, 
          t2.ERICD3X, 
          t2.ERPRO1X, 
          t2.ERCCC1X, 
          t2.ERCCC2X, 
          t2.ERCCC3X, 
          t2.FFERTYPE, 
          t2.FFBEF12, 
          t2.ERXP12X, 
          t2.ERTC12X, 
          t2.ERFSF12X, 
          t2.ERFMR12X, 
          t2.ERFMD12X, 
          t2.ERFPV12X, 
          t2.ERFVA12X, 
          t2.ERFTR12X, 
          t2.ERFOF12X, 
          t2.ERFSL12X, 
          t2.ERFWC12X, 
          t2.ERFOR12X, 
          t2.ERFOU12X, 
          t2.ERFOT12X, 
          t2.ERFXP12X, 
          t2.ERFTC12X, 
          t2.ERDSF12X, 
          t2.ERDMR12X, 
          t2.ERDMD12X, 
          t2.ERDPV12X, 
          t2.ERDVA12X, 
          t2.ERDTR12X, 
          t2.ERDOF12X, 
          t2.ERDSL12X, 
          t2.ERDWC12X, 
          t2.ERDOR12X, 
          t2.ERDOU12X, 
          t2.ERDOT12X, 
          t2.ERDXP12X, 
          t2.ERDTC12X, 
          t2.IMPFLAG, 
          t2.PERWT12F, 
          t2.VARSTR, 
          t2.VARPSU, 
          t2.IN_ER
      FROM MYDATA.INFULLYR_VARIABLE t1
           FULL JOIN MYDATA.IN_ER_VARIABLE t2 ON (t1.DUPERSID = t2.DUPERSID)
      WHERE t2.IN_ER = 1 AND t1.INFULLYR = 1;
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: FIRST 100 OBS.   */
%LET _CLIENTTASKLABEL='FIRST 100 OBS.';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:51 PM
   By task: FIRST 100 OBS.

   Input Data: Local:MYDATA.MERGER_FOR_FULLYR_INER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MERGER_FOR_FULLYR_INER
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.IN_ER, T.DUPERSID1, T.DUPERSID
	FROM MYDATA.MERGER_FOR_FULLYR_INER(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Report Listing";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";

PROC PRINT DATA=WORK.SORTTempTableSorted
	(OBS=100)
	OBS="Row number"
	LABEL
	;
	VAR IN_ER DUPERSID1 DUPERSID;
RUN;
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


/*   START OF NODE: Data Set Attributes   */
%LET _CLIENTTASKLABEL='Data Set Attributes';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:52 PM
   By task: Data Set Attributes

   Input Data: Local:MYDATA.MERGER_FOR_FULLYR_INER
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(MYDATA.CONTCONTENTSFORMERGER_FOR_FULLYR);
TITLE;
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FORMAT;
   VALUE _EG_VARTYPE 1="Numeric" 2="Character" OTHER="unknown";
RUN;

PROC DATASETS NOLIST NODETAILS; 
   CONTENTS DATA=MYDATA.MERGER_FOR_FULLYR_INER OUT=WORK.SUCOUT1;

RUN;

DATA MYDATA.CONTCONTENTSFORMERGER_FOR_FULLYR(LABEL="Contents Details for MERGER_FOR_FULLYR_INER");
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
			typemem LABEL="Data Set Type" FROM MYDATA.CONTCONTENTSFORMERGER_FOR_FULLYR
	ORDER BY memname ; 

CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM WORK.SCVIEW
		WHERE memname='MERGER_FOR_FULLYR_INER';
QUIT;

TITLE "Tables on &_SASSERVERNAME"; 
PROC REPORT DATA=WORK.SCTABLE; 
   DEFINE  MEMLABEL / DISPLAY WIDTH=&_LINESIZE; 
   COLUMN memname memlabel memtype crdate modate nobs charset protect typemem; 
RUN;QUIT;

PROC SORT DATA=MYDATA.CONTCONTENTSFORMERGER_FOR_FULLYR OUT=MYDATA.CONTCONTENTSFORMERGER_FOR_FULLYR;
   BY memname name;
RUN;

OPTIONS NOBYLINE;
TITLE 'Variables in Table: #BYVAL(memname)'; 

PROC SQL;
DROP TABLE WORK.SCTABLE;
CREATE TABLE WORK.SCTABLE AS
	SELECT * FROM MYDATA.CONTCONTENTSFORMERGER_FOR_FULLYR
		WHERE memname='MERGER_FOR_FULLYR_INER';
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


/*   START OF NODE: MERGED_DATA_RECODED   */
%LET _CLIENTTASKLABEL='MERGED_DATA_RECODED';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MERGED_DATA_RECODED);

PROC SQL;
   CREATE TABLE MYDATA.MERGED_DATA_RECODED(label="MERGED_DATA_RECODED") AS 
   SELECT t1.DUPERSID, 
          t1.Aggregate_SF12_Catagorical, 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          t1.Aggregate_SUM_Rescored, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.IN_ER, 
          /* XRAY_RECODED */
            (CASE 
               WHEN -7 = t1.XRAYS THEN .
               WHEN -8 = t1.XRAYS THEN .
               WHEN -9 = t1.XRAYS THEN .
               WHEN 95 = t1.XRAYS THEN 2
               ELSE t1.XRAYS
            END) LABEL="XRAY RECODED" AS XRAY_RECODED, 
          /* MRI_RECODED */
            (CASE 
               WHEN -7 = t1.MRI THEN .
               WHEN -8 = t1.MRI THEN .
               WHEN -9 = t1.MRI THEN .
               WHEN 95 = t1.MRI THEN 2
               ELSE t1.MRI
            END) LABEL="MRI RECODED" AS MRI_RECODED
      FROM MYDATA.MERGER_FOR_FULLYR_INER t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies   */
%LET _CLIENTTASKLABEL='One-Way Frequencies';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:54 PM
   By task: One-Way Frequencies

   Input Data: Local:MYDATA.MERGED_DATA_RECODED
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MERGED_DATA_RECODED
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.XRAY_RECODED, T.MRI_RECODED
	FROM MYDATA.MERGED_DATA_RECODED(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES XRAY_RECODED /  SCORES=TABLE;
	TABLES MRI_RECODED /  SCORES=TABLE;
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


/*   START OF NODE: Count Variable   */
%LET _CLIENTTASKLABEL='Count Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.COUNT_VARIABLE);

PROC SQL;
   CREATE TABLE MYDATA.COUNT_VARIABLE(label="COUNT_VARIABLE") AS 
   SELECT t1.DUPERSID, 
          /* COUNT_of_DUPERSID */
            (COUNT(t1.DUPERSID)) AS COUNT_of_DUPERSID
      FROM MYDATA.MERGED_DATA_RECODED t1
      GROUP BY t1.DUPERSID;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Merged Data Recoded with Count   */
%LET _CLIENTTASKLABEL='Merged Data Recoded with Count';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.MERGED_RECODED_COUNT);

PROC SQL;
   CREATE TABLE MYDATA.MERGED_RECODED_COUNT(label="MERGED_RECODED_COUNT") AS 
   SELECT t1.DUPERSID, 
          t1.Aggregate_SF12_Catagorical, 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          t1.Aggregate_SUM_Rescored, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.IN_ER, 
          t1.XRAY_RECODED, 
          t1.MRI_RECODED, 
          t2.DUPERSID AS DUPERSID2, 
          t2.COUNT_of_DUPERSID
      FROM MYDATA.MERGED_DATA_RECODED t1
           INNER JOIN MYDATA.COUNT_VARIABLE t2 ON (t1.DUPERSID = t2.DUPERSID);
QUIT;

GOPTIONS NOACCESSIBLE;



%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: One-Way Frequencies of Count   */
%LET _CLIENTTASKLABEL='One-Way Frequencies of Count';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:56 PM
   By task: One-Way Frequencies of Count

   Input Data: Local:MYDATA.MERGED_RECODED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MERGED_RECODED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.COUNT_of_DUPERSID
	FROM MYDATA.MERGED_RECODED_COUNT(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID /  SCORES=TABLE;
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


/*   START OF NODE: Histogram of Count Variable   */
%LET _CLIENTTASKLABEL='Histogram of Count Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:57 PM
   By task: Histogram of Count Variable

   Input Data: Local:MYDATA.MERGED_RECODED_COUNT
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   PROC SHEWHART does not support DEVICE=ACTIVEX. Switching to PNG.
   ------------------------------------------------------------------- */
OPTIONS DEV=PNG;
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.MERGED_RECODED_COUNT
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID
	FROM MYDATA.MERGED_RECODED_COUNT(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Distribution analysis of: COUNT_of_DUPERSID";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
	ODS EXCLUDE EXTREMEOBS MODES MOMENTS;
	
	GOPTIONS htext=1 cells;
	SYMBOL v=SQUARE c=BLUE h=1 cells;
	PATTERN v=SOLID
	;
PROC UNIVARIATE DATA = WORK.SORTTempTableSorted
		CIBASIC(TYPE=TWOSIDED ALPHA=0.05)
		MU0=0
;
	VAR COUNT_of_DUPERSID;
	HISTOGRAM   COUNT_of_DUPERSID / NORMAL	( 	W=1 	L=1 	COLOR=YELLOW  MU=EST SIGMA=EST)
	
		CFRAME=GRAY CAXES=BLACK WAXIS=1  CBARLINE=BLACK CFILL=BLUE PFILL=SOLID ;
	;
/* -------------------------------------------------------------------
   End of task code.
   ------------------------------------------------------------------- */
RUN; QUIT;
%_eg_conditional_dropds(WORK.SORTTempTableSorted);
TITLE; FOOTNOTE;
/* -------------------------------------------------------------------
   Restoring original device type setting.
   ------------------------------------------------------------------- */
OPTIONS DEV=ACTIVEX;


GOPTIONS NOACCESSIBLE;
%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Count Categorical   */
%LET _CLIENTTASKLABEL='Count Categorical';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
%_eg_conditional_dropds(MYDATA.COUNT_CATEGORICAL);

PROC SQL;
   CREATE TABLE MYDATA.COUNT_CATEGORICAL(label="COUNT_CATEGORICAL") AS 
   SELECT t1.DUPERSID, 
          t1.Aggregate_SF12_Catagorical, 
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
          t1.ADGENH42_RECODED, 
          t1.ADDAYA42_RECODED, 
          t1.ADCLIM42_RECODED, 
          t1.ADPALS42_RECODED, 
          t1.ADPWLM42_RECODED, 
          t1.ADMALS42_RECODED, 
          t1.ADMWLM42_RECODED, 
          t1.ADPAIN421_RECODED, 
          t1.ADCAPE42_RECODED, 
          t1.ADNRGY42_RECODED, 
          t1.ADDOWN42_RECODED, 
          t1.ADSOCA42_RECODED, 
          t1.ADAPPT42_RECODED, 
          t1.ADDPRS42_RECODED, 
          t1.ADDRBP42_RECODED, 
          t1.ADEFERT42_RECODED, 
          t1.ADEGMC42_RECODED, 
          t1.ADEXPL42_RECODED, 
          t1.ADEZUN42_RECODED, 
          t1.ADFHLP42_RECODED, 
          t1.ADHOPE42_RECODED, 
          t1.ADREST42_RECODED, 
          t1.BENDIF53_RECODED, 
          t1.LFTDIF53_RECODED, 
          t1.RCHDIF53_RECODED, 
          t1.STNDIF53_RECODED, 
          t1.WLKDIF53_RECODED, 
          t1.BRSTEX53_RECODED, 
          t1.JTPAIN53_RECODED, 
          t1.ACTDTY42_RECODED, 
          t1.STPDIF53_RECODED, 
          t1.MAMOGR53_RECODED, 
          t1.ADGENH42_RESCORED, 
          t1.ADPAIN42_RESCORED, 
          t1.ADCAPE42_RESCORED, 
          t1.ADNRGY42_RESCORED, 
          t1.Aggregate_SUM_Rescored, 
          t1.INFULLYR, 
          t1.DUID, 
          t1.PID, 
          t1.DUPERSID1, 
          t1.EVNTIDX, 
          t1.EVENTRN, 
          t1.ERHEVIDX, 
          t1.FFEEIDX, 
          t1.PANEL, 
          t1.MPCDATA, 
          t1.ERDATEYR, 
          t1.ERDATEMM, 
          t1.ERDATEDD, 
          t1.SEEDOC, 
          t1.VSTCTGRY, 
          t1.VSTRELCN, 
          t1.LABTEST, 
          t1.SONOGRAM, 
          t1.XRAYS, 
          t1.MAMMOG, 
          t1.MRI, 
          t1.EKG, 
          t1.EEG, 
          t1.RCVVAC, 
          t1.ANESTH, 
          t1.THRTSWAB, 
          t1.OTHSVCE, 
          t1.SURGPROC, 
          t1.MEDPRESC, 
          t1.ERICD1X, 
          t1.ERICD2X, 
          t1.ERICD3X, 
          t1.ERPRO1X, 
          t1.ERCCC1X, 
          t1.ERCCC2X, 
          t1.ERCCC3X, 
          t1.FFERTYPE, 
          t1.FFBEF12, 
          t1.ERXP12X, 
          t1.ERTC12X, 
          t1.ERFSF12X, 
          t1.ERFMR12X, 
          t1.ERFMD12X, 
          t1.ERFPV12X, 
          t1.ERFVA12X, 
          t1.ERFTR12X, 
          t1.ERFOF12X, 
          t1.ERFSL12X, 
          t1.ERFWC12X, 
          t1.ERFOR12X, 
          t1.ERFOU12X, 
          t1.ERFOT12X, 
          t1.ERFXP12X, 
          t1.ERFTC12X, 
          t1.ERDSF12X, 
          t1.ERDMR12X, 
          t1.ERDMD12X, 
          t1.ERDPV12X, 
          t1.ERDVA12X, 
          t1.ERDTR12X, 
          t1.ERDOF12X, 
          t1.ERDSL12X, 
          t1.ERDWC12X, 
          t1.ERDOR12X, 
          t1.ERDOU12X, 
          t1.ERDOT12X, 
          t1.ERDXP12X, 
          t1.ERDTC12X, 
          t1.IMPFLAG, 
          t1.PERWT12F, 
          t1.VARSTR, 
          t1.VARPSU, 
          t1.IN_ER, 
          t1.XRAY_RECODED, 
          t1.MRI_RECODED, 
          t1.DUPERSID2, 
          t1.COUNT_of_DUPERSID, 
          /* ER_Count_Categorical */
            (CASE  
               WHEN t1.COUNT_of_DUPERSID = 1
               THEN 1
               WHEN t1.COUNT_of_DUPERSID >= 2 and t1.COUNT_of_DUPERSID < 4
               THEN 2
               ELSE 3
            END) LABEL="ER Visits Counted Categorical" AS ER_Count_Categorical
      FROM MYDATA.MERGED_RECODED_COUNT t1;
QUIT;

GOPTIONS NOACCESSIBLE;


%LET _CLIENTTASKLABEL=;
%LET _CLIENTPROJECTPATH=;
%LET _CLIENTPROJECTNAME=;


/*   START OF NODE: Comparison of Categorical and Count Variables   */
%LET _CLIENTTASKLABEL='Comparison of Categorical and Count Variables';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:59 PM
   By task: Comparison of Categorical and Count Variables

   Input Data: Local:MYDATA.COUNT_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORTTempTableSorted);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.COUNT_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORTTempTableSorted AS
		SELECT T.COUNT_of_DUPERSID, T.ER_Count_Categorical
	FROM MYDATA.COUNT_CATEGORICAL(FIRSTOBS=1 ) as T
;
QUIT;
TITLE;
TITLE1 "Table Analysis";
TITLE2 "Results For Comparison of Categorical and Original Variables";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.)) By John Fought";
PROC FREQ DATA = WORK.SORTTempTableSorted
	ORDER=INTERNAL
;
	TABLES COUNT_of_DUPERSID * ER_Count_Categorical /
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


/*   START OF NODE: One-Way Frequencies For Categorical Variable   */
%LET _CLIENTTASKLABEL='One-Way Frequencies For Categorical Variable';
%LET _CLIENTPROJECTPATH='\\Client\P$\QAC\qac200\students\jfought\Assignments\FoughtJ_SAS_project_Jan 13 2015.egp';
%LET _CLIENTPROJECTNAME='FoughtJ_SAS_project_Jan 13 2015.egp';

GOPTIONS ACCESSIBLE;
/* -------------------------------------------------------------------
   Code generated by SAS Task

   Generated on: Tuesday, January 13, 2015 at 1:30:59 PM
   By task: One-Way Frequencies For Categorical Variable

   Input Data: Local:MYDATA.COUNT_CATEGORICAL
   Server:  Local
   ------------------------------------------------------------------- */

%_eg_conditional_dropds(WORK.SORT);
/* -------------------------------------------------------------------
   Sort data set Local:MYDATA.COUNT_CATEGORICAL
   ------------------------------------------------------------------- */

PROC SQL;
	CREATE VIEW WORK.SORT AS
		SELECT T.ER_Count_Categorical
	FROM MYDATA.COUNT_CATEGORICAL(FIRSTOBS=1 ) as T
;
QUIT;

TITLE;
TITLE1 "One-Way Frequencies";
TITLE2 "Results For Categorical Variable";
FOOTNOTE;
FOOTNOTE1 "Generated by the SAS System (&_SASSERVERNAME, &SYSSCPL) on %TRIM(%QSYSFUNC(DATE(), NLDATE20.)) at %TRIM(%SYSFUNC(TIME(), TIMEAMPM12.))";
PROC FREQ DATA=WORK.SORT
	ORDER=INTERNAL
;
	TABLES ER_Count_Categorical /  SCORES=TABLE;
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

;*';*";*/;quit;run;
ODS _ALL_ CLOSE;
