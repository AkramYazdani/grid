#include "grid.h"

int isUnitArithmetic(SEXP ua) {
    return inherits(ua, "unit.arithmetic");
}

int isUnitList(SEXP ul) {
    return inherits(ul, "unit.list");
}

/* Function to build a single-value unit SEXP internally.
 * Cannot build units requiring data as yet.
 */
SEXP unit(double value, int unit) 
{
    SEXP u, units, classname;
    PROTECT(u = allocVector(REALSXP, 1));
    REAL(u)[0] = value;
    PROTECT(units = allocVector(INTSXP, 1));
    INTEGER(units)[0] = unit;
    /* NOTE that we do not set the "unit" attribute */
    setAttrib(u, install("valid.unit"), units);
    setAttrib(u, install("data"), R_NilValue);
    PROTECT(classname = allocVector(STRSXP, 1));
    SET_STRING_ELT(classname, 0, mkChar("unit"));
    classgets(u, classname);
    UNPROTECT(3);
    return u;
}

/* Accessor functions for unit objects
 */

/* 
 * This is an attempt to extract a single numeric value from
 * a unit.  This is ONLY designed for use on "simple" units
 * (i.e., NOT unitLists or unitArithmetics)
 */
static double unitValue(SEXP unit, int index) {
    /* Recycle values if necessary (used in unit arithmetic)
     */
    int n = LENGTH(unit);
    return numeric(unit, index % n);
}

int unitUnit(SEXP unit, int index) {
    SEXP units = getAttrib(unit, install("valid.unit"));
    /* Recycle units if necessary 
     */
    int n = LENGTH(units);
    return INTEGER(units)[index % n];
}

SEXP unitData(SEXP unit, int index) {
    SEXP result;
    SEXP data = getAttrib(unit, install("data"));
    if (isNull(data))
	result = R_NilValue;
    else {
	/* Recycle data if necessary 
	 */
	int n = LENGTH(data);
	result = VECTOR_ELT(data, index % n);
    }
    return result;
}

/* Accessor functions for unit arithmetic object
 */
char* fName(SEXP ua) {
    return CHAR(STRING_ELT(getListElement(ua, "fname"), 0));
}

SEXP arg1(SEXP ua) {
    return getListElement(ua, "arg1");
}

SEXP arg2(SEXP ua) {
    return getListElement(ua, "arg2");
}

int fNameMatch(SEXP ua, char *aString) {
    return !strcmp(fName(ua), aString);
}

int addOp(SEXP ua) {
    return fNameMatch(ua, "+");
}

int minusOp(SEXP ua) {
    return fNameMatch(ua, "-");
}

int timesOp(SEXP ua) {
    return fNameMatch(ua, "*");
}

int fOp(SEXP ua) {
    return addOp(ua) || minusOp(ua) || timesOp(ua);
}

int minFunc(SEXP ua) {
    return fNameMatch(ua, "min");
}
    
int maxFunc(SEXP ua) {
    return fNameMatch(ua, "max");
}

int sumFunc(SEXP ua) {
    return fNameMatch(ua, "sum");
}

/* Functions in lattice.c should use this to determine the length
 * of a unit/unitArithmetic object rather than just LENGTH.
 */
int unitLength(SEXP u) 
{
    int result = 0;
    if (isUnitList(u))
	result = LENGTH(u);
    else if (isUnitArithmetic(u))
	if (fOp(u)) {
	    if (timesOp(u)) 
		result = unitLength(arg2(u));
	    else {  /* must be "+" or "-" */
		int n1 = unitLength(arg1(u));
		int n2 = unitLength(arg2(u));
		result = (n1 > n2) ? n1 : n2;
	    }
	} else /* must be "min" or "max" or "sum" */
	    result = unitLength(arg1(u));
    else /* Must be a unit object */
	result = LENGTH(u);
    return result;
}


/**************************
 * Code for handling "null" units
 **************************
 */

/* Global mode indicators:
 * The value returned for a "null" unit depends on ...
 * (i) whether layout is calling for evaluation of a "pure null" unit
 *     (in which case, the value of the "null" unit is returned)
 * (ii) the sort of arithmetic that is being performed
 *      (in which case, an "identity" value is returned)
 */
/* This declared in lattice.h
 */
int L_nullLayoutMode = 0;

int L_nullArithmeticMode;

/* 
 * Evaluate a "null" _value_ dependent on the evaluation context
 */
static double evaluateNullUnit(double value) {
    double result = value;
    if (!L_nullLayoutMode)
	switch (L_nullArithmeticMode) {
	case L_plain:
	case L_adding:
	case L_subtracting:
	case L_summing:
	    result = 0;
	    break;
	case L_multiplying:
	    result = 1;
	    break;
	case L_maximising:
	    result = 0;
	    break;
	case L_minimising:
	    result = 1;
	    break;
	}
    return result;
}

/*
 * Evaluate a "null" _unit_ 
 * This is used by layout code to get a single "null" _value_
 * from a pureNullUnit (which may be a unitList or a unitArithmetic)
 *
 * This must ONLY be called on a unit which has passed the 
 * pureNullUnit test below.
 */
double pureNullUnitValue(SEXP unit, int index)
{
    double result = 0;
    if (isUnitArithmetic(unit)) {
	int i;
	if (addOp(unit)) {
	    result = unitValue(arg1(unit), index) + 
		unitValue(arg2(unit), index);
	}
	else if (minusOp(unit)) {
	    result = unitValue(arg1(unit), index) - 
		unitValue(arg2(unit), index);
	}
	else if (timesOp(unit)) {
	    result = REAL(arg1(unit))[index] * 
		unitValue(arg2(unit), index);
	}
	else if (minFunc(unit)) {
	    int n = unitLength(arg1(unit));
	    double temp = DBL_MAX;
	    result = unitValue(arg1(unit), 0);
	    for (i=1; i<n; i++) {
		temp = unitValue(arg1(unit), i);
		if (temp < result)
		    result = temp;
	    }
	} 
	else if (maxFunc(unit)) {
	    int n = unitLength(arg1(unit));
	    double temp = DBL_MIN;
	    result = unitValue(arg1(unit), 0);
	    for (i=1; i<n; i++) {
		temp = unitValue(arg1(unit), i);
		if (temp > result)
		    result = temp;
	    }
	} 
	else if (sumFunc(unit)) {
	    int n = unitLength(arg1(unit));
	    result = 0.0;
	    for (i=0; i<n; i++) {
		result += unitValue(arg1(unit), i);
	    }
	}
	else 
	    error("Unimplemented unit function");
    } else if (isUnitList(unit)) {
	result = unitValue(VECTOR_ELT(unit, index), 0);
    } else
	result = unitValue(unit, index);
    return result;
}

int pureNullUnitArithmetic(SEXP unit, int index);

int pureNullUnit(SEXP unit, int index) {
    int result;
    if (isUnitArithmetic(unit)) 
	result = pureNullUnitArithmetic(unit, index);
    else if (isUnitList(unit)) {
	result = pureNullUnit(VECTOR_ELT(unit, index), 0);
    } else {  /* Just a plain unit */
	/* Special case:  if "grobwidth" or "grobheight" unit
	 * and width/height(grob) is pure null
	 */
	if (unitUnit(unit, index) == L_GROBWIDTH) {
	    SEXP fn, R_fcall;
	    SEXP width;
	    PROTECT(fn = findFun(install("width"), R_GlobalEnv));
	    PROTECT(R_fcall = lang2(fn, unitData(unit, index)));
	    PROTECT(width = eval(R_fcall, R_GlobalEnv));
	    result = pureNullUnit(width, 0);
	    UNPROTECT(3);
	} else if (unitUnit(unit, index) == L_GROBHEIGHT) {
	    SEXP fn, R_fcall;
	    SEXP height;
	    PROTECT(fn = findFun(install("height"), R_GlobalEnv));
	    PROTECT(R_fcall = lang2(fn, unitData(unit, index)));
	    PROTECT(height = eval(R_fcall, R_GlobalEnv));
	    result = pureNullUnit(height, 0);
	    UNPROTECT(3);
	} else
	    result = unitUnit(unit, index) == L_NULL;
    }
    return result;    
}

int pureNullUnitArithmetic(SEXP unit, int index) {
    int result;
    if (addOp(unit) || minusOp(unit)) {
	result = pureNullUnit(arg1(unit), index) &&
	    pureNullUnit(arg2(unit), index);
    }
    else if (timesOp(unit)) {
	result = pureNullUnit(arg2(unit), index);
    }
    else if (minFunc(unit) || maxFunc(unit) || sumFunc(unit)) {
	int n = unitLength(arg1(unit));
	int i = 0;
	result = 1;
	while (result && i<n) {
	    result = result && pureNullUnit(arg1(unit), i);
	    i += 1;
	}
    } 
    else 
	error("Unimplemented unit function");
    return result;
}

/**************************
 * Code for handling "grobwidth" units
 **************************
 */

/* NOTE:  this code calls back to R code to perform 
 * set.gpar operations, which will impact on grid global variables
 * BUT that's ok(ish) because it subsequently calls back to R code to perform
 * corresponding unset.gpar operations which will
 * undo the changes to the grid globals
 */

double evaluateGrobWidthUnit(SEXP grob, char* vpfontfamily, int vpfont,
			     double vpfontsize, double vplineheight,
			     double vpwidthCM, double vpheightCM,
			     GEDevDesc *dd) 
{
    /* FIXME:  I probably want to create a new environment here
     * rather than use the global environment (?) 
     * Ditto in three eval()s below.
     */
    SEXP widthPreFn, widthFn, widthPostFn, R_fcall1, R_fcall2, R_fcall3;
    SEXP getGParFn, R_getgparcall, gparname;
    SEXP width, fontfamily, font, fontsize, lineheight;
    LViewportContext vpc;
    double resultINCHES, result;
    PROTECT(widthPreFn = findFun(install("width.pre"), R_GlobalEnv));
    PROTECT(widthFn = findFun(install("width"), R_GlobalEnv));
    PROTECT(widthPostFn = findFun(install("width.post"), R_GlobalEnv));
    PROTECT(getGParFn = findFun(install("get.gpar"), R_GlobalEnv));
    /* Call width.pre(grob) 
     */
    PROTECT(R_fcall1 = lang2(widthPreFn, grob));
    eval(R_fcall1, R_GlobalEnv);
    /* Call width(grob)
     * to get the unit representing the with
     */
    PROTECT(R_fcall2 = lang2(widthFn, grob));
    PROTECT(width = eval(R_fcall2, R_GlobalEnv));
    /* Call get.gpar() to get the current fontsize and lineheight settings
     */
    PROTECT(gparname = allocVector(STRSXP, 1));
    SET_STRING_ELT(gparname, 0, mkChar("fontfamily"));
    PROTECT(R_getgparcall = lang2(getGParFn, gparname));
    PROTECT(fontfamily = eval(R_getgparcall, R_GlobalEnv));
    SET_STRING_ELT(gparname, 0, mkChar("font"));
    R_getgparcall = lang2(getGParFn, gparname);
    PROTECT(font = eval(R_getgparcall, R_GlobalEnv));
    SET_STRING_ELT(gparname, 0, mkChar("fontsize"));
    R_getgparcall = lang2(getGParFn, gparname);
    PROTECT(fontsize = eval(R_getgparcall, R_GlobalEnv));
    SET_STRING_ELT(gparname, 0, mkChar("lineheight"));
    R_getgparcall = lang2(getGParFn, gparname);
    PROTECT(lineheight = eval(R_getgparcall, R_GlobalEnv));
    /* Transform the width
     * NOTE:  the width.pre function should NOT have done any 
     * viewport pushing so the viewport context and sizeCM 
     * should be unchanged
     * NOTE:  We transform into INCHES so can produce final answer in terms
     * of NPC for original context
     */
    /* Special case for "null" units
     */
    if (unitUnit(width, 0) == L_NULL) {
	result = evaluateNullUnit(unitValue(width, 0));
    } else {
	/* These are the current fontsize and lineheight.
	 * They are adequate (both in the sense of being the correct 
	 * values and in the sense of being all the vpc information 
	 * that is required) ONLY because of the strict
	 * restrictions placed on what sort of units can
	 * be used in a width.details method.
	 * If those restrictions are lifted, all bets are off ...
	 */
	vpc.fontfamily = vpfontfamily;
	vpc.font = vpfont;
	vpc.fontsize = vpfontsize;
	vpc.lineheight = vplineheight;
	resultINCHES = transformWidthtoINCHES(width, 0, vpc,
					      CHAR(STRING_ELT(fontfamily, 0)),
					      INTEGER(font)[0],
					      REAL(fontsize)[0], 
					      REAL(lineheight)[0],
					      /* These are the current
					       * viewport widthCM and
					       * heightCM.
					       * They are adequate ONLY
					       * because of the strict
					       * restrictions placed on
					       * what sort of units can
					       * be used in a width.details
					       * method.
					       * If those restrictions are
					       * lifted, all bets are off ...
					       */
					      vpwidthCM, vpheightCM,
					      dd);
	result = resultINCHES/(vpwidthCM/2.54);
    }
    /* Call width.post(grob)
     */
    PROTECT(R_fcall3 = lang2(widthPostFn, grob));
    eval(R_fcall3, R_GlobalEnv);
    UNPROTECT(14);
    /* Return the transformed width
     */
    return result;
}

/* See evaluateGrobWidthUnit for detailed comments
 */
double evaluateGrobHeightUnit(SEXP grob, char *vpfontfamily, int vpfont, 
			     double vpfontsize, double vplineheight,
			     double vpheightCM, double vpwidthCM,
			     GEDevDesc *dd) 
{
    /* FIXME:  I probably want to create a new environment here
     * rather than use the global environment (?) 
     * Ditto in three eval()s below.
     */
    SEXP heightPreFn, heightFn, heightPostFn, R_fcall1, R_fcall2, R_fcall3;
    SEXP getGParFn, R_getgparcall, gparname;
    SEXP height, fontfamily, font, fontsize, lineheight;
    LViewportContext vpc;
    double resultINCHES, result;
    PROTECT(heightPreFn = findFun(install("height.pre"), R_GlobalEnv));
    PROTECT(heightFn = findFun(install("height"), R_GlobalEnv));
    PROTECT(heightPostFn = findFun(install("height.post"), R_GlobalEnv));
    PROTECT(getGParFn = findFun(install("get.gpar"), R_GlobalEnv));
    /* Call height.pre(grob) 
     */
    PROTECT(R_fcall1 = lang2(heightPreFn, grob));
    eval(R_fcall1, R_GlobalEnv);
    /* Call height(grob)
     * to get the unit representing the with
     */
    PROTECT(R_fcall2 = lang2(heightFn, grob));
    PROTECT(height = eval(R_fcall2, R_GlobalEnv));
    /* Call get.gpar() to get the current fontsize and lineheight settings
     */
    PROTECT(gparname = allocVector(STRSXP, 1));
    SET_STRING_ELT(gparname, 0, mkChar("fontfamily"));
    PROTECT(R_getgparcall = lang2(getGParFn, gparname));
    PROTECT(fontfamily = eval(R_getgparcall, R_GlobalEnv));
    SET_STRING_ELT(gparname, 0, mkChar("font"));
    R_getgparcall = lang2(getGParFn, gparname);
    PROTECT(font = eval(R_getgparcall, R_GlobalEnv));
    SET_STRING_ELT(gparname, 0, mkChar("fontsize"));
    R_getgparcall = lang2(getGParFn, gparname);
    PROTECT(fontsize = eval(R_getgparcall, R_GlobalEnv));
    SET_STRING_ELT(gparname, 0, mkChar("lineheight"));
    R_getgparcall = lang2(getGParFn, gparname);
    PROTECT(lineheight = eval(R_getgparcall, R_GlobalEnv));
    /* Transform the height
     * NOTE:  the height.pre function should NOT have done any 
     * viewport pushing so the viewport context and sizeCM 
     * should be unchanged
     * NOTE:  We transform into INCHES so can produce final answer in terms
     * of NPC for original context
     */
    /* Special case for "null" units
     */
    if (unitUnit(height, 0) == L_NULL) {
	result = evaluateNullUnit(unitValue(height, 0));
    } else {
	vpc.fontfamily = vpfontfamily;
	vpc.font = vpfont;
	vpc.fontsize = vpfontsize;
	vpc.lineheight = vplineheight;
	resultINCHES = transformHeighttoINCHES(height, 0, vpc,
					       CHAR(STRING_ELT(fontfamily, 0)),
					       INTEGER(font)[0],
					       REAL(fontsize)[0], 
					       REAL(lineheight)[0],
					       vpwidthCM, vpheightCM,
					       dd);
	result = resultINCHES/(vpheightCM/2.54);
    }
    /* Call height.post(grob)
     */
    PROTECT(R_fcall3 = lang2(heightPostFn, grob));
    eval(R_fcall3, R_GlobalEnv);
    UNPROTECT(14);
    /* Return the transformed height
     */
    return result;
}

/**************************
 * TRANSFORMATIONS
 **************************
 */
    
/* Map a value from arbitrary units to Normalised Parent Coordinates */

double transform(double value, int unit, SEXP data,
		 double scalemin, double scalemax,
		 char *fontfamily, int font, double fontsize, 
		 double lineheight, 
		 char *grobfontfamily, int grobfont, double grobfontSize, 
		 double groblineHeight, 
		 double thisCM, double otherCM,
		 GEDevDesc *dd)
{
    double result = value;
    switch (unit) {
    case L_NPC:      
	break;
    case L_CM: 
	result = result/thisCM;
	break;
    case L_INCHES: 
        result = result/(thisCM/2.54);
	break;
    /* FIXME:  The following two assume that the pointsize specified
     * by the user is actually the pointsize provided by the
     * device.  This is NOT a safe assumption
     * One possibility would be to do a call to GReset(), just so
     * that mapping() gets called, just so that things like
     * xNDCPerLine are up-to-date, THEN call GStrHeight("M")
     * or somesuch.
     */
    case L_CHAR:
	result = result*fontsize/(72*thisCM/2.54);
	break;	
    case L_MYCHAR:
	result = result*grobfontSize/(72*thisCM/2.54);
	break;	
    case L_LINES:
	result = result*fontsize*lineheight/(72*thisCM/2.54);
	break;
    case L_MYLINES:
	result = result*grobfontSize*groblineHeight/(72*thisCM/2.54);
	break;
    case L_SNPC:        
	if (thisCM <= otherCM)
	    result = result;
	else                     
	    result = result*(otherCM/thisCM);
	break;
    case L_MM:
	result = result/10/thisCM;
	break;
	/* Maybe an opportunity for some constants below here (!) 
	 */
    case L_POINTS:
	result = result/72.27*2.54/thisCM;
	break;
    case L_PICAS:
	result = result*12/72.27*2.54/thisCM;
	break;
    case L_BIGPOINTS:
	result = result/72*2.54/thisCM; 
	break;
    case L_DIDA:
	result = result/1157*1238/72.27*2.54/thisCM;
	break;
    case L_CICERO:
	result = result*12/1157*1238/72.27*2.54/thisCM;
	break;
    case L_SCALEDPOINTS:
	result = result*65536/72.27*2.54/thisCM;
	break;
    case L_STRINGWIDTH:
	if (isExpression(data))
	    result = result*
		fromDeviceWidth(GEExpressionWidth(VECTOR_ELT(data, 0),
						  font, 1, fontsize, dd),
				GE_INCHES, dd)*
		2.54/thisCM;
	else
	    result = result*
		fromDeviceWidth(GEStrWidth(CHAR(STRING_ELT(data, 0)), 
					   fontfamily, font, lineheight,
					   1, fontsize, dd),
				GE_INCHES, dd)*
		2.54/thisCM;
	break;
    case L_MYSTRINGWIDTH:
	if (isExpression(data))
	    result = result*
		fromDeviceWidth(GEExpressionWidth(VECTOR_ELT(data, 0),
						  grobfont, 1, 
						  grobfontSize, dd),
				GE_INCHES, dd)*
		2.54/thisCM;
	else
	    result = result*
		fromDeviceWidth(GEStrWidth(CHAR(STRING_ELT(data, 0)), 
					   grobfontfamily, grobfont, lineheight,
					   1, grobfontSize, dd),
				GE_INCHES, dd)*
		2.54/thisCM;
	break;
    case L_STRINGHEIGHT:
	if (isExpression(data))
	    result = result*
		fromDeviceHeight(GEExpressionHeight(VECTOR_ELT(data, 0),
						    font, 1, fontsize, dd),
				 GE_INCHES, dd)*
		2.54/thisCM;
	else
	    result = result*
		fromDeviceHeight(GEStrHeight(CHAR(STRING_ELT(data, 0)),
					     fontfamily, font, lineheight,
					     1, fontsize, dd),
				 GE_INCHES, dd)*
		2.54/thisCM;
	break;
    case L_MYSTRINGHEIGHT:
	if (isExpression(data))
	    result = result*
		fromDeviceHeight(GEExpressionHeight(VECTOR_ELT(data, 0),
						    grobfont, 1, 
						    grobfontSize, dd),
				 GE_INCHES, dd)*
		2.54/thisCM;
	else
	    result = result*
		fromDeviceHeight(GEStrHeight(CHAR(STRING_ELT(data, 0)),
					     grobfontfamily, grobfont, lineheight,
					     1, grobfontSize, dd),
				 GE_INCHES, dd)*
		2.54/thisCM;
	break;
    case L_GROBWIDTH:
	result = value*evaluateGrobWidthUnit(data, fontfamily, 
					     font, fontsize, lineheight,
					     thisCM, otherCM, dd);
	break;
    case L_GROBHEIGHT:
	result = value*evaluateGrobHeightUnit(data, fontfamily,
					      font, fontsize, lineheight,
					      thisCM, otherCM, dd);
	break;
    case L_NULL:
	result = evaluateNullUnit(result);
	break;
    default:
	error("Illegal unit or unit not yet implemented");
    }
    return result;
}

/* FIXME:  scales are only linear at the moment */
double transformLocation(double location, int unit, SEXP data,
			 double scalemin, double scalemax,
			 char *fontfamily, int font, double fontsize, 
			 double lineheight, 
			 char *grobfontfamily, int grobfont, 
			 double grobfontSize, double groblineHeight, 
			 double thisCM, double otherCM,
			 GEDevDesc *dd)
{
    double result = location;
    switch (unit) {
    case L_NATIVE:       
	result = (result - scalemin)/(scalemax - scalemin);
	break;
    default:
	result = transform(location, unit, data, scalemin, scalemax,
			   fontfamily, font, fontsize, lineheight, 
			   grobfontfamily, grobfont, 
			   grobfontSize, groblineHeight, 
			   thisCM, otherCM, dd);
    }
    return result;
}

double transformXArithmetic(SEXP x, int index,
			    LViewportContext vpc,
			    char *fontfamily, int font, double fontsize, 
			    double lineheight,
			    double widthCM, double heightCM,
			    GEDevDesc *dd);

double transformX(SEXP x, int index,
		  LViewportContext vpc,
		  char *fontfamily, int font, double fontsize, 
		  double lineheight,
		  double widthCM, double heightCM,
		  GEDevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(x)) 
	result = transformXArithmetic(x, index, vpc, 
				      fontfamily, font, fontsize, lineheight,
				      widthCM, heightCM, dd);
    else if (isUnitList(x)) {
	int n = unitLength(x);
	result = transformX(VECTOR_ELT(x, index % n), 0, vpc, 
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
    } else {  /* Just a plain unit */
	L_nullArithmeticMode = L_plain;
	result = unitValue(x, index);
	unit = unitUnit(x, index);
	data = unitData(x, index);
	result = transformLocation(result, unit, data, 
				   vpc.xscalemin, vpc.xscalemax,
				   vpc.fontfamily, vpc.font, 
				   vpc.fontsize, vpc.lineheight,
				   fontfamily, font, fontsize, lineheight,
				   widthCM, heightCM, dd);
    }
    return result;
}

double transformXtoNative(SEXP x, int index,
			  LViewportContext vpc,
			  char *fontfamily, int font, double fontsize, 
			  double lineheight,
			  double widthCM, double heightCM,
			  GEDevDesc *dd)
{
    return vpc.xscalemin + 
	transformX(x, index, vpc, fontfamily, font, fontsize, lineheight, 
		   widthCM, heightCM, dd)*
	(vpc.xscalemax - vpc.xscalemin);
}

double transformYArithmetic(SEXP y, int index,
			    LViewportContext vpc,
			    char *fontfamily, int font, double fontsize, 
			    double lineheight,
			    double widthCM, double heightCM,
			    GEDevDesc *dd);

double transformY(SEXP y, int index, 
		  LViewportContext vpc,
		  char *fontfamily, int font, double fontsize, 
		  double lineheight,
		  double widthCM, double heightCM,
		  GEDevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(y))
	result = transformYArithmetic(y, index, vpc,
				      fontfamily, font, fontsize, lineheight,
				      widthCM, heightCM, dd);
    else if (isUnitList(y)) {
	int n = unitLength(y);
	result = transformY(VECTOR_ELT(y, index % n), 0, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
    } else { /* Just a unit object */
	L_nullArithmeticMode = L_plain;
	result = unitValue(y, index);
	unit = unitUnit(y, index);
	data = unitData(y, index);
	result = transformLocation(result, unit, data, 
				   vpc.yscalemin, vpc.yscalemax,
				   vpc.fontfamily, vpc.font, 
				   vpc.fontsize, vpc.lineheight,
				   fontfamily, font, fontsize, lineheight,
				   heightCM, widthCM, dd);
    } 
    return result;
}

double transformYtoNative(SEXP y, int index,
			  LViewportContext vpc,
			  char *fontfamily, int font, double fontsize, 
			  double lineheight,
			  double widthCM, double heightCM,
			  GEDevDesc *dd)
{
    return vpc.yscalemin + 
	transformY(y, index, vpc, fontfamily, font, fontsize, lineheight, 
		   widthCM, heightCM, dd)*
	(vpc.yscalemax - vpc.yscalemin);
}

double transformDimension(double dim, int unit, SEXP data,
			  double scalemin, double scalemax,
			  char *fontfamily, int font, double fontsize, 
			  double lineheight, 
			  char *grobfontfamily, int grobfont, 
			  double grobfontSize, double groblineHeight, 
			  double thisCM, double otherCM,
			  GEDevDesc *dd)
{
    double result = dim;
    switch (unit) {
    case L_NATIVE:
	result = (dim)/(scalemax - scalemin);
	break;
    default:
	result = transform(dim, unit, data, scalemin, scalemax,
			   fontfamily, font, fontsize, lineheight, 
			   grobfontfamily, grobfont, 
			   grobfontSize, groblineHeight, 
			   thisCM, otherCM, dd);
    }
    return result;
}

double transformWidthArithmetic(SEXP width, int index,
				LViewportContext vpc,
				char *fontfamily, int font, double fontsize, 
				double lineheight, 
				double widthCM, double heightCM,
				GEDevDesc *dd);

double transformWidth(SEXP width, int index, 
		      LViewportContext vpc,
		      char *fontfamily, int font, double fontsize, 
		      double lineheight,
		      double widthCM, double heightCM,
		      GEDevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(width))
	result = transformWidthArithmetic(width, index, vpc,
					  fontfamily, font, fontsize, 
					  lineheight, 
					  widthCM, heightCM, dd);
    else if (isUnitList(width)) {
	int n = unitLength(width);
	result = transformWidth(VECTOR_ELT(width, index % n), 0, vpc,
				fontfamily, font, fontsize, 
				lineheight, 
				widthCM, heightCM, dd);
    } else { /* Just a unit object */
	L_nullArithmeticMode = L_plain;
	result = unitValue(width, index);
	unit = unitUnit(width, index);
	data = unitData(width, index);
	result = transformDimension(result, unit, data, 
				    vpc.xscalemin, vpc.xscalemax,
				    vpc.fontfamily, vpc.font, 
				    vpc.fontsize, vpc.lineheight,
				    fontfamily, font, fontsize, 
				    lineheight, 
				    widthCM, heightCM, dd);
    }
    return result;
}

double transformWidthtoNative(SEXP width, int index,
			      LViewportContext vpc,
			      char *fontfamily, int font, double fontsize, 
			      double lineheight,
			      double widthCM, double heightCM,
			      GEDevDesc *dd)
{
    return transformWidth(width, index, vpc, fontfamily, font, fontsize, 
			  lineheight, 
			  widthCM, heightCM, dd)*
	(vpc.xscalemax - vpc.xscalemin);
}

double transformHeightArithmetic(SEXP height, int index,
				 LViewportContext vpc,
				 char *fontfamily, int font, double fontsize, 
				 double lineheight,
				 double widthCM, double heightCM,
				 GEDevDesc *dd);

double transformHeight(SEXP height, int index,
		       LViewportContext vpc,
		       char *fontfamily, int font, double fontsize, 
		       double lineheight,
		       double widthCM, double heightCM,
		       GEDevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(height))
	result = transformHeightArithmetic(height, index, vpc,
					   fontfamily, font, fontsize, 
					   lineheight, 
					   widthCM, heightCM, dd);
    else if (isUnitList(height)) {
	int n = unitLength(height);
	result = transformHeight(VECTOR_ELT(height, index % n), 0, vpc,
				 fontfamily, font, fontsize, 
				 lineheight, 
				 widthCM, heightCM, dd);
    } else { /* Just a unit object */
	L_nullArithmeticMode = L_plain;
	result = unitValue(height, index);
	unit = unitUnit(height, index);
	data = unitData(height, index);
	result = transformDimension(result, unit, data, 
				    vpc.yscalemin, vpc.yscalemax,
				    vpc.fontfamily, vpc.font, 
				    vpc.fontsize, vpc.lineheight,
				    fontfamily, font, fontsize, 
				    lineheight, 
				    heightCM, widthCM, dd);
    }
    return result;
}

double transformHeighttoNative(SEXP height, int index,
			       LViewportContext vpc,
			       char *fontfamily, int font, double fontsize, 
			       double lineheight,
			       double widthCM, double heightCM,
			       GEDevDesc *dd)
{
    return transformHeight(height, index, vpc, fontfamily, font, fontsize, 
			   lineheight, 
			   widthCM, heightCM, dd)*
	(vpc.yscalemax - vpc.yscalemin);
}

double transformXArithmetic(SEXP x, int index,
			    LViewportContext vpc,
			    char *fontfamily, int font, double fontsize, 
			    double lineheight,
			    double widthCM, double heightCM,
			    GEDevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(x)) {
	L_nullArithmeticMode = L_adding;
	result = transformX(arg1(x), index, vpc,
			    fontfamily, font, fontsize, 
			    lineheight, 
			    widthCM, heightCM, dd) +
	    transformX(arg2(x), index, vpc,
		       fontfamily, font, fontsize, 
		       lineheight, 
		       widthCM, heightCM, dd);
    }
    else if (minusOp(x)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformX(arg1(x), index, vpc,
			    fontfamily, font, fontsize, 
			    lineheight, 
			    widthCM, heightCM, dd) -
	    transformX(arg2(x), index, vpc,
		       fontfamily, font, fontsize, 
		       lineheight, 
		       widthCM, heightCM, dd);
    }
    else if (timesOp(x)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(x))[0] *
	    transformX(arg2(x), index, vpc,
		       fontfamily, font, fontsize, 
		       lineheight, 
		       widthCM, heightCM, dd);
    }
    else if (minFunc(x)) {
	int n = unitLength(arg1(x));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformX(arg1(x), 0, vpc,
			    fontfamily, font, fontsize, 
			    lineheight, 
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformX(arg1(x), i, vpc,
			      fontfamily, font, fontsize, 
			      lineheight, 
			      widthCM, heightCM, dd);
	    if (temp < result)
		result = temp;
	}
    } 
    else if (maxFunc(x)) {
	int n = unitLength(arg1(x));
	double temp = DBL_MIN;
	L_nullArithmeticMode = L_maximising;
	result = transformX(arg1(x), 0, vpc,
			    fontfamily, font, fontsize, 
			    lineheight, 
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformX(arg1(x), i, vpc,
			      fontfamily, font, fontsize, 
			      lineheight, 
			      widthCM, heightCM, dd);
	    if (temp > result)
		result = temp;
	}
    }
    else if (sumFunc(x)) {
	int n = unitLength(arg1(x));
	result = 0.0;
	L_nullArithmeticMode = L_summing;
	for (i=0; i<n; i++) {
	    result += transformX(arg1(x), i, vpc,
				 fontfamily, font, fontsize, 
				 lineheight, 
				 widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

double transformYArithmetic(SEXP y, int index,
			    LViewportContext vpc,
			    char *fontfamily, int font, double fontsize, 
			    double lineheight,
			    double widthCM, double heightCM,
			    GEDevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(y)) {
	L_nullArithmeticMode = L_adding;
	result = transformY(arg1(y), index, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd) +
	    transformY(arg2(y), index, vpc,
		       fontfamily, font, fontsize, lineheight,
		       widthCM, heightCM, dd);
    }
    else if (minusOp(y)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformY(arg1(y), index, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd) -
	    transformY(arg2(y), index, vpc,
		       fontfamily, font, fontsize, lineheight,
		       widthCM, heightCM, dd);
    }
    else if (timesOp(y)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(y))[0] *
	    transformY(arg2(y), index, vpc,
		       fontfamily, font, fontsize, lineheight,
		       widthCM, heightCM, dd);
    }
    else if (minFunc(y)) {
	int n = unitLength(arg1(y));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformY(arg1(y), 0, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformY(arg1(y), i, vpc,
			      fontfamily, font, fontsize, lineheight,
			      widthCM, heightCM, dd);
	    if (temp < result)
		result = temp;
	}
    } 
    else if (maxFunc(y)) {
	int n = unitLength(arg1(y));
	double temp = DBL_MIN;
	L_nullArithmeticMode = L_maximising;
	result = transformY(arg1(y), 0, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformY(arg1(y), i, vpc,
			      fontfamily, font, fontsize, lineheight,
			      widthCM, heightCM, dd);
	    if (temp > result)
		result = temp;
	}
    }
    else if (sumFunc(y)) {
	int n = unitLength(arg1(y));
	L_nullArithmeticMode = L_summing;
	result = 0.0;
	for (i=0; i<n; i++) {
	    result += transformY(arg1(y), i, vpc,
				 fontfamily, font, fontsize, lineheight,
				 widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

double transformWidthArithmetic(SEXP width, int index,
				LViewportContext vpc,
				char *fontfamily, int font, double fontsize, 
				double lineheight,
				double widthCM, double heightCM,
				GEDevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(width)) {
	L_nullArithmeticMode = L_adding;
	result = transformWidth(arg1(width), index, vpc,
				fontfamily, font, fontsize, lineheight,
				widthCM, heightCM, dd) +
	    transformWidth(arg2(width), index, vpc,
			   fontfamily, font, fontsize, lineheight,
			   widthCM, heightCM, dd);
    }
    else if (minusOp(width)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformWidth(arg1(width), index, vpc,
				fontfamily, font, fontsize, lineheight,
				widthCM, heightCM, dd) -
	    transformWidth(arg2(width), index, vpc,
			   fontfamily, font, fontsize, lineheight,
			   widthCM, heightCM, dd);
    }
    else if (timesOp(width)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(width))[0] *
	    transformWidth(arg2(width), index, vpc,
			   fontfamily, font, fontsize, lineheight,
			   widthCM, heightCM, dd);
    }
    else if (minFunc(width)) {
	int n = unitLength(arg1(width));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformWidth(arg1(width), 0, vpc,
				fontfamily, font, fontsize, lineheight,
				widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformWidth(arg1(width), i, vpc,
				  fontfamily, font, fontsize, lineheight,
				  widthCM, heightCM, dd);
	    if (temp < result)
		result = temp;
	}
    } 
    else if (maxFunc(width)) {
	int n = unitLength(arg1(width));
	double temp = DBL_MIN;
	L_nullArithmeticMode = L_maximising;
	result = transformWidth(arg1(width), 0, vpc,
				fontfamily, font, fontsize, lineheight,
				widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformWidth(arg1(width), i, vpc,
				  fontfamily, font, fontsize, lineheight,
				  widthCM, heightCM, dd);
	    if (temp > result)
		result = temp;
	}
    }
    else if (sumFunc(width)) {
	int n = unitLength(arg1(width));
	result = 0.0;
	L_nullArithmeticMode = L_summing;
	for (i=0; i<n; i++) {
	    result += transformWidth(arg1(width), i, vpc,
				     fontfamily, font, fontsize, lineheight,
				     widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

double transformHeightArithmetic(SEXP height, int index,
				 LViewportContext vpc,
				 char *fontfamily, int font, double fontsize, 
				 double lineheight,
				 double widthCM, double heightCM,
				 GEDevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(height)) {
	L_nullArithmeticMode = L_adding;
	result = transformHeight(arg1(height), index, vpc,
				 fontfamily, font, fontsize, lineheight,
				 widthCM, heightCM, dd) +
	    transformHeight(arg2(height), index, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
    }
    else if (minusOp(height)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformHeight(arg1(height), index, vpc,
				 fontfamily, font, fontsize, lineheight,
				 widthCM, heightCM, dd) -
	    transformHeight(arg2(height), index, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
    }
    else if (timesOp(height)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(height))[0] *
	    transformHeight(arg2(height), index, vpc,
			    fontfamily, font, fontsize, lineheight,
			    widthCM, heightCM, dd);
    }
    else if (minFunc(height)) {
	int n = unitLength(arg1(height));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformHeight(arg1(height), 0, vpc,
				 fontfamily, font, fontsize, lineheight,
				 widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformHeight(arg1(height), i, vpc,
				   fontfamily, font, fontsize, lineheight,
				   widthCM, heightCM, dd);
	    if (temp < result)
		result = temp;
	}
    } 
    else if (maxFunc(height)) {
	int n = unitLength(arg1(height));
	double temp = DBL_MIN;
	L_nullArithmeticMode = L_maximising;
	result = transformHeight(arg1(height), 0, vpc,
				 fontfamily, font, fontsize, lineheight,
				 widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformHeight(arg1(height), i, vpc,
				   fontfamily, font, fontsize, lineheight,
				   widthCM, heightCM, dd);
	    if (temp > result)
		result = temp;
	}
    }
    else if (sumFunc(height)) {
	int n = unitLength(arg1(height));
	L_nullArithmeticMode = L_summing;
	result = 0.0;
	for (i=0; i<n; i++) {
	    result += transformHeight(arg1(height), i, vpc,
				      fontfamily, font, fontsize, lineheight,
				      widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

/* Code for transforming a location in INCHES using a transformation matrix.
 * We work in INCHES so that rotations can be incorporated within the
 * transformation matrix (i.e., the units are the same in both x- and
 * y-directions).
 * INCHES rather than CM because the R graphics engine only has INCHES.
 */

/* The original transform[X | Y | Width | Height] functions
 * were written to transform to NPC.  Rather than muck with them,
 * I am just wrappering them to get the new transformation to INCHES
 * In other words, the reason for the apparent inefficiency here
 * is historical.
 */

/* The difference between transform*toINCHES and transformLocn/Dimn 
 * is that the former are just converting from one coordinate system
 * to INCHES;  the latter are converting from INCHES relative to
 * the parent to INCHES relative to the device.
 */
double transformXtoINCHES(SEXP x, int index, 
			  LViewportContext vpc,
			  char *fontfamily, int font, double fontsize, 
			  double lineheight,
			  double widthCM, double heightCM,
			  GEDevDesc *dd)
{
    return transformX(x, index, vpc, fontfamily, font, fontsize, lineheight,
		      widthCM, heightCM, dd)*widthCM/2.54;
}

double transformYtoINCHES(SEXP y, int index, 
			  LViewportContext vpc,
			  char *fontfamily, int font, double fontsize, 
			  double lineheight,
			  double widthCM, double heightCM,
			  GEDevDesc *dd)
{
    return transformY(y, index, vpc, fontfamily, font, fontsize, lineheight,
		      widthCM, heightCM, dd)*heightCM/2.54;
}

void transformLocn(SEXP x, SEXP y, int index, 
		   LViewportContext vpc,
		   char *fontfamily, int font, double fontsize, 
		   double lineheight,
		   double widthCM, double heightCM,
		   GEDevDesc *dd,
		   LTransform t,
		   double *xx, double *yy)
{
    LLocation lin, lout;
    /* x and y are unit objects (i.e., values in any old coordinate
     * system) so the first step is to convert them both to CM
     */
    *xx = transformXtoINCHES(x, index, vpc, fontfamily, font, fontsize, 
			     lineheight,
			     widthCM, heightCM, dd);
    *yy = transformYtoINCHES(y, index, vpc, fontfamily, font, fontsize, 
			     lineheight,
			     widthCM, heightCM, dd);
    location(*xx, *yy, lin);
    trans(lin, t, lout);
    *xx = locationX(lout);
    *yy = locationY(lout);
}

double transformWidthtoINCHES(SEXP w, int index,
			      LViewportContext vpc,
			      char *fontfamily, int font, double fontsize, 
			      double lineheight,
			      double widthCM, double heightCM,
			      GEDevDesc *dd)
{
    return transformWidth(w, index, vpc, fontfamily, font, fontsize, 
			  lineheight,
			  widthCM, heightCM, dd)*widthCM/2.54;
}

double transformHeighttoINCHES(SEXP h, int index,
			       LViewportContext vpc,
			       char *fontfamily, int font, double fontsize, 
			       double lineheight,
			       double widthCM, double heightCM,
			       GEDevDesc *dd)
{
    return transformHeight(h, index, vpc, fontfamily, font, fontsize, 
			   lineheight,
			   widthCM, heightCM, dd)*heightCM/2.54;
}

void transformDimn(SEXP w, SEXP h, int index, 
		   LViewportContext vpc,
		   char *fontfamily, int font, double fontsize, 
		   double lineheight,
		   double widthCM, double heightCM,
		   GEDevDesc *dd,
		   double rotationAngle,
		   double *ww, double *hh)
{
    LLocation din, dout;
    LTransform r;
    *ww = transformWidthtoINCHES(w, index, vpc, fontfamily, font, fontsize, 
				 lineheight,
				 widthCM, heightCM, dd);
    *hh = transformHeighttoINCHES(h, index, vpc, fontfamily, font, fontsize, 
				  lineheight,
				  widthCM, heightCM, dd);
    location(*ww, *hh, din);
    rotation(rotationAngle, r);
    trans(din, r, dout);
    *ww = locationX(dout);
    *hh = locationY(dout);
}


/* Attempt to make validating units faster
 */
typedef struct {
    char *name;
    int code;
} UnitTab;

/* NOTE this table must match the order in grid.h
 */
static UnitTab UnitTable[] = {
    { "npc",            0 },
    { "cm",             1 },
    { "inches",         2 },
    { "lines",          3 },
    { "native",         4 },
    { "null",           5 },
    { "snpc",           6 },
    { "mm",             7 },
    { "points",         8 },
    { "picas",          9 },
    { "bigpts",        10 },
    { "dida",          11 },
    { "cicero",        12 },
    { "scaledpts",     13 },
    { "strwidth",      14 },
    { "strheight",     15 },

    { "char",          18 },
    { "grobwidth",     19 },
    { "grobheight",    20 },
    { "mylines",       21 },
    { "mychar",        22 },
    { "mystrwidth",        23 },
    { "mystrheight",        24 },

    { NULL,            -1 }
};

int convertUnit(SEXP unit, int index) 
{
    int i = 0;
    int result = 0;
    int found = 0;
    while (result >= 0 && !found) {
	if (UnitTable[i].name == NULL) 
	    result = -1;
	else {
	    found = !strcmp(CHAR(STRING_ELT(unit, index)), UnitTable[i].name);
	    if (found) 
		result = UnitTable[i].code;
	}
	i += 1;
    }
    if (result < 0)
	error("Invalid unit");
    return result;
}
	    
SEXP validUnits(SEXP units) 
{
    int i;
    int n = LENGTH(units);
    SEXP answer;
    PROTECT(answer = allocVector(INTSXP, n));
    for (i = 0; i<n; i++) 
	INTEGER(answer)[i] = convertUnit(units, i);
    UNPROTECT(1);
    return answer;
}
    


