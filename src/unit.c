#include "grid.h"

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
double unitValue(SEXP unit, int index) {
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

int isUnitArithmetic(SEXP ua) {
    return inherits(ua, "unit.arithmetic");
}

int isUnitList(SEXP ul) {
    return inherits(ul, "unit.list");
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

int pureNullUnitArithmetic(SEXP unit, int index);

int pureNullUnit(SEXP unit, int index) {
    int result;
    if (isUnitArithmetic(unit)) 
	result = pureNullUnitArithmetic(unit, index);
    else if (isUnitList(unit)) {
	result = pureNullUnit(VECTOR_ELT(unit, index), 0);
    } else {  /* Just a plain unit */
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
	int i;
	result = 1;
	for (i=0; i<n; i++) 
	    result = result && pureNullUnit(arg1(unit), i);
    } 
    else 
	error("Unimplemented unit function");
    return result;
}

/**************************
 * TRANSFORMATIONS
 **************************
 */
    
/* Map a value from arbitrary units to Normalised Parent Coordinates */

double transform(double value, int unit, SEXP data,
		 double scalemin, double scalemax,
		 double fontSize, double lineHeight,
		 double thisCM, double otherCM,
		 DevDesc *dd)
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
	result = result*fontSize/(72*thisCM/2.54);
	break;	
    case L_LINES:
	result = result*fontSize*lineHeight/(72*thisCM/2.54);
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
	result = result*GStrWidth(CHAR(STRING_ELT(data, 0)), INCHES, dd)*
	    2.54/thisCM;
	break;
    case L_STRINGHEIGHT:
	result = result*GStrHeight(CHAR(STRING_ELT(data, 0)), INCHES, dd)*
	    2.54/thisCM;
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
			 double fontSize, double lineHeight,
			 double thisCM, double otherCM,
			 DevDesc *dd)
{
    double result = location;
    switch (unit) {
    case L_NATIVE:       
	result = (result - scalemin)/(scalemax - scalemin);
	break;
    default:
	result = transform(location, unit, data, scalemin, scalemax,
			   fontSize, lineHeight, thisCM, otherCM, dd);
    }
    return result;
}

double transformXArithmetic(SEXP x, int index,
			    LViewportContext vpc,
			    double widthCM, double heightCM,
			    DevDesc *dd);

double transformX(SEXP x, int index,
		  LViewportContext vpc,
		  double widthCM, double heightCM,
		  DevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(x)) 
	result = transformXArithmetic(x, index, vpc, widthCM, heightCM, dd);
    else if (isUnitList(x)) {
	int n = unitLength(x);
	result = transformX(VECTOR_ELT(x, index % n), 0, vpc, 
			    widthCM, heightCM, dd);
    } else {  /* Just a plain unit */
	L_nullArithmeticMode = L_plain;
	result = unitValue(x, index);
	unit = unitUnit(x, index);
	data = unitData(x, index);
	result = transformLocation(result, unit, data, 
				   vpc.xscalemin, vpc.xscalemax,
				   vpc.fontsize, vpc.lineheight,
				   widthCM, heightCM, dd);
	switch (vpc.origin) {
	case L_BOTTOMLEFT:         
	case L_TOPLEFT:    
	    break;
	case L_BOTTOMRIGHT:        
	case L_TOPRIGHT: 
	    result = 1 - result;
	    break;
	}
    }
    return result;
}

double transformYArithmetic(SEXP y, int index,
			    LViewportContext vpc,
			    double widthCM, double heightCM,
			    DevDesc *dd);

double transformY(SEXP y, int index, 
		  LViewportContext vpc,
		  double widthCM, double heightCM,
		  DevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(y))
	result = transformYArithmetic(y, index, vpc,
				      widthCM, heightCM, dd);
    else if (isUnitList(y)) {
	int n = unitLength(y);
	result = transformY(VECTOR_ELT(y, index % n), 0, vpc,
			    widthCM, heightCM, dd);
    } else { /* Just a unit object */
	L_nullArithmeticMode = L_plain;
	result = unitValue(y, index);
	unit = unitUnit(y, index);
	data = unitData(y, index);
	result = transformLocation(result, unit, data, 
				   vpc.yscalemin, vpc.yscalemax,
				   vpc.fontsize, vpc.lineheight,
				   heightCM, widthCM, dd);
	switch (vpc.origin) {
	case L_BOTTOMLEFT:
	case L_BOTTOMRIGHT:
	    break;          
	case L_TOPLEFT:    
	case L_TOPRIGHT:   
	    result = 1 - result;
	    break;
	}
    } 
    return result;
}

double transformDimension(double dim, int unit, SEXP data,
			  double scalemin, double scalemax,
			  double fontSize, double lineHeight,
			  double thisCM, double otherCM,
			  DevDesc *dd)
{
    double result = dim;
    switch (unit) {
    case L_NATIVE:
	result = (dim)/(scalemax - scalemin);
	break;
    default:
	result = transform(dim, unit, data, scalemin, scalemax,
			   fontSize, lineHeight, thisCM, otherCM, dd);
    }
    return result;
}

double transformWidthArithmetic(SEXP width, int index,
				LViewportContext vpc,
				double widthCM, double heightCM,
				DevDesc *dd);

double transformWidth(SEXP width, int index, 
		      LViewportContext vpc,
		      double widthCM, double heightCM,
		      DevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(width))
	result = transformWidthArithmetic(width, index, vpc,
					  widthCM, heightCM, dd);
    else if (isUnitList(width)) {
	int n = unitLength(width);
	result = transformWidth(VECTOR_ELT(width, index % n), 0, vpc,
				widthCM, heightCM, dd);
    } else { /* Just a unit object */
	L_nullArithmeticMode = L_plain;
	result = unitValue(width, index);
	unit = unitUnit(width, index);
	data = unitData(width, index);
	result = transformDimension(result, unit, data, 
				    vpc.xscalemin, vpc.xscalemax,
				    vpc.fontsize, vpc.lineheight,
				    widthCM, heightCM, dd);
	switch (vpc.origin) {
	case L_BOTTOMLEFT:
	case L_TOPLEFT: 
	    break;
	case L_BOTTOMRIGHT:
	case L_TOPRIGHT:  
	    result = -result;
	    break;
	}
    }
    return result;
}

double transformHeightArithmetic(SEXP height, int index,
				 LViewportContext vpc,
				 double widthCM, double heightCM,
				 DevDesc *dd);

double transformHeight(SEXP height, int index,
		       LViewportContext vpc,
		       double widthCM, double heightCM,
		       DevDesc *dd)
{
    double result;
    int unit;
    SEXP data;
    if (isUnitArithmetic(height))
	result = transformHeightArithmetic(height, index, vpc,
					   widthCM, heightCM, dd);
    else if (isUnitList(height)) {
	int n = unitLength(height);
	result = transformHeight(VECTOR_ELT(height, index % n), 0, vpc,
				 widthCM, heightCM, dd);
    } else { /* Just a unit object */
	L_nullArithmeticMode = L_plain;
	result = unitValue(height, index);
	unit = unitUnit(height, index);
	data = unitData(height, index);
	result = transformDimension(result, unit, data, 
				    vpc.yscalemin, vpc.yscalemax,
				    vpc.fontsize, vpc.lineheight,
				    heightCM, widthCM, dd);
	switch (vpc.origin) {
	case L_BOTTOMLEFT:
	case L_BOTTOMRIGHT:
	    break;
	case L_TOPLEFT:
	case L_TOPRIGHT: 
	    result = -result;
	    break;
	}
    }
    return result;
}

double transformXArithmetic(SEXP x, int index,
			    LViewportContext vpc,
			    double widthCM, double heightCM,
			    DevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(x)) {
	L_nullArithmeticMode = L_adding;
	result = transformX(arg1(x), index, vpc,
			    widthCM, heightCM, dd) +
	    transformX(arg2(x), index, vpc,
		       widthCM, heightCM, dd);
    }
    else if (minusOp(x)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformX(arg1(x), index, vpc,
			    widthCM, heightCM, dd) -
	    transformX(arg2(x), index, vpc,
		       widthCM, heightCM, dd);
    }
    else if (timesOp(x)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(x))[0] *
	    transformX(arg2(x), index, vpc,
		       widthCM, heightCM, dd);
    }
    else if (minFunc(x)) {
	int n = unitLength(arg1(x));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformX(arg1(x), 0, vpc,
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformX(arg1(x), i, vpc,
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
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformX(arg1(x), i, vpc,
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
				 widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

double transformYArithmetic(SEXP y, int index,
			    LViewportContext vpc,
			    double widthCM, double heightCM,
			    DevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(y)) {
	L_nullArithmeticMode = L_adding;
	result = transformY(arg1(y), index, vpc,
			    widthCM, heightCM, dd) +
	    transformY(arg2(y), index, vpc,
		       widthCM, heightCM, dd);
    }
    else if (minusOp(y)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformY(arg1(y), index, vpc,
			    widthCM, heightCM, dd) -
	    transformY(arg2(y), index, vpc,
		       widthCM, heightCM, dd);
    }
    else if (timesOp(y)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(y))[0] *
	    transformY(arg2(y), index, vpc,
		       widthCM, heightCM, dd);
    }
    else if (minFunc(y)) {
	int n = unitLength(arg1(y));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformY(arg1(y), 0, vpc,
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformY(arg1(y), i, vpc,
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
			    widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformY(arg1(y), i, vpc,
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
				 widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

double transformWidthArithmetic(SEXP width, int index,
				LViewportContext vpc,
				double widthCM, double heightCM,
				DevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(width)) {
	L_nullArithmeticMode = L_adding;
	result = transformWidth(arg1(width), index, vpc,
				widthCM, heightCM, dd) +
	    transformWidth(arg2(width), index, vpc,
			   widthCM, heightCM, dd);
    }
    else if (minusOp(width)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformWidth(arg1(width), index, vpc,
				widthCM, heightCM, dd) -
	    transformWidth(arg2(width), index, vpc,
			   widthCM, heightCM, dd);
    }
    else if (timesOp(width)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(width))[0] *
	    transformWidth(arg2(width), index, vpc,
			   widthCM, heightCM, dd);
    }
    else if (minFunc(width)) {
	int n = unitLength(arg1(width));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformWidth(arg1(width), 0, vpc,
				widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformWidth(arg1(width), i, vpc,
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
				widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformWidth(arg1(width), i, vpc,
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
				     widthCM, heightCM, dd);
	}
    }
    else 
	error("Unimplemented unit function");
    return result;
}

double transformHeightArithmetic(SEXP height, int index,
				 LViewportContext vpc,
				 double widthCM, double heightCM,
				 DevDesc *dd)
{
    int i;
    double result = 0;
    if (addOp(height)) {
	L_nullArithmeticMode = L_adding;
	result = transformHeight(arg1(height), index, vpc,
				 widthCM, heightCM, dd) +
	    transformHeight(arg2(height), index, vpc,
			    widthCM, heightCM, dd);
    }
    else if (minusOp(height)) {
	L_nullArithmeticMode = L_subtracting;
	result = transformHeight(arg1(height), index, vpc,
				 widthCM, heightCM, dd) -
	    transformHeight(arg2(height), index, vpc,
			    widthCM, heightCM, dd);
    }
    else if (timesOp(height)) {
	L_nullArithmeticMode = L_multiplying;
	result = REAL(arg1(height))[0] *
	    transformHeight(arg2(height), index, vpc,
			    widthCM, heightCM, dd);
    }
    else if (minFunc(height)) {
	int n = unitLength(arg1(height));
	double temp = DBL_MAX;
	L_nullArithmeticMode = L_minimising;
	result = transformHeight(arg1(height), 0, vpc,
				 widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformHeight(arg1(height), i, vpc,
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
				 widthCM, heightCM, dd);
	for (i=1; i<n; i++) {
	    temp = transformHeight(arg1(height), i, vpc,
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
double transformXtoINCHES(SEXP x, int index, 
			  LViewportContext vpc,
			  double widthCM, double heightCM,
			  DevDesc *dd)
{
    return transformX(x, index, vpc, widthCM, heightCM, dd)*widthCM/2.54;
}

double transformYtoINCHES(SEXP y, int index, 
			  LViewportContext vpc,
			  double widthCM, double heightCM,
			  DevDesc *dd)
{
    return transformY(y, index, vpc, widthCM, heightCM, dd)*heightCM/2.54;
}

void transformLocn(SEXP x, SEXP y, int index, 
		   LViewportContext vpc,
		   double widthCM, double heightCM,
		   DevDesc *dd,
		   LTransform t,
		   double *xx, double *yy)
{
    LLocation lin, lout;
    /* x and y are unit objects (i.e., values in any old coordinate
     * system) so the first step is to convert them both to CM
     */
    *xx = transformXtoINCHES(x, index, vpc, widthCM, heightCM, dd);
    *yy = transformYtoINCHES(y, index, vpc, widthCM, heightCM, dd);
    location(*xx, *yy, lin);
    trans(lin, t, lout);
    *xx = locationX(lout);
    *yy = locationY(lout);
}

double transformWidthtoINCHES(SEXP w, int index,
			      LViewportContext vpc,
			      double widthCM, double heightCM,
			      DevDesc *dd)
{
    return transformWidth(w, index, vpc, widthCM, heightCM, dd)*widthCM/2.54;
}

double transformHeighttoINCHES(SEXP h, int index,
			       LViewportContext vpc,
			       double widthCM, double heightCM,
			       DevDesc *dd)
{
    return transformHeight(h, index, vpc, widthCM, heightCM, dd)*heightCM/2.54;
}

void transformDimn(SEXP w, SEXP h, int index, 
		   LViewportContext vpc,
		   double widthCM, double heightCM,
		   DevDesc *dd,
		   double rotationAngle,
		   double *ww, double *hh)
{
    LLocation din, dout;
    LTransform r;
    *ww = transformWidthtoINCHES(w, index, vpc,
				 widthCM, heightCM, dd);
    *hh = transformHeighttoINCHES(h, index, vpc,
				  widthCM, heightCM, dd);
    location(*ww, *hh, din);
    rotation(rotationAngle, r);
    trans(din, r, dout);
    *ww = locationX(dout);
    *hh = locationY(dout);
}




