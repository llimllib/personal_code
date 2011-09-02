/******************************************************************************
 * $Id: shp2text.c,v 1.7 2005/11/12 01:44:31 bryce Exp $
 *
 * Project:  Shapelib
 * Purpose:  Dump shapefile and database contents into text formats
 * Author:   Bryce Nessbitt, based on work of Frank Warmerdam, warmerdam@pobox.com
 *
 * Bugs:	 TODO:: Exports illegal characters to xml (e.g. &)
 *
 ******************************************************************************
 * Copyright (c) 1999, Frank Warmerdam
 *
 * This software is available under the following "MIT Style" license,
 * or at the option of the licensee under the LGPL (see LICENSE.LGPL).  This
 * option is discussed in more detail in shapelib.html.
 *
 * --
 * 
 * Permission is hereby granted, free of charge, to any person obtaining a
 * copy of this software and associated documentation files (the "Software"),
 * to deal in the Software without restriction, including without limitation
 * the rights to use, copy, modify, merge, publish, distribute, sublicense,
 * and/or sell copies of the Software, and to permit persons to whom the
 * Software is furnished to do so, subject to the following conditions:
 *
 * The above copyright notice and this permission notice shall be included
 * in all copies or substantial portions of the Software.
 *
 * THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND, EXPRESS
 * OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF MERCHANTABILITY,
 * FITNESS FOR A PARTICULAR PURPOSE AND NONINFRINGEMENT. IN NO EVENT SHALL
 * THE AUTHORS OR COPYRIGHT HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER
 * LIABILITY, WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING
 * FROM, OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
 * DEALINGS IN THE SOFTWARE.
 ******************************************************************************
 */

static char rcsid[] = 
  "$Id: shp2text.c,v 1.7 2005/11/12 01:44:31 bryce Exp $";

#include <stdlib.h>
#include <strings.h>
#include <string.h>
#include "shapefil.h"

int main( int argc, char ** argv )

{
    SHPHandle	hSHP;
    DBFHandle   hDBF;

    int		nShapeType, nEntities, i, iPart, nInvalidCount=0;
    char * bFormat;
    const char 	*pszPlus;
    double 	adfMinBound[4], adfMaxBound[4];
    int     *panWidth, iRecord;
    char    szFormat[32], *pszFilename = NULL;
    int     nWidth, nDecimals;
    int     bHeader = 0;
    int     bRaw = 0;
    int     bMultiLine = 0;
    int     dEntities, dFields;
    char    szTitle[12];

    char    *argFilename,*argTitle=NULL;
    int     argNameField=0,argAttributeField=0;

    char *  temp1;
    char *  temp2;

/* -------------------------------------------------------------------- */
/*      Parse arguments, Display a usage message.                       */
/* -------------------------------------------------------------------- */
    bFormat = "text";
    if( argc > 1 && strcmp(argv[1],"--spreadsheet") == 0 )
    {
        bFormat = "spreadsheet";
        argv++;
        argc--;
    }
    if( argc > 1 && strcmp(argv[1],"--geo.position") == 0 )
    {
        bFormat = "geo.position";
        argv++;
        argc--;
    }
    else if( argc > 1 && strcmp(argv[1],"--gpx") == 0 )
    {
        bFormat = "gpx";
        argv++;
        argc--;
        if( argc == 4 )
        {
        argNameField  = atoi(argv[2]);
        argAttributeField = atoi(argv[3]);
        } else {
        printf( "shp2text --gpx shape_file.shp name_field# attribute_field#\n" );
        printf( ";utility to dump esri shapefiles into various text formats\n");
        printf( ";you must supply two field numbers\n");
        if( argc < 2 ) {
            exit( 5 );
            }
        bFormat = "fields";     // Try to print field numbers if a filename was given
        }
    }
    else if( argc != 2 )
    {
        printf( "shp2text [--gpx][--spreadsheet][--geo.position] shape_file.shp\n" );
        printf( ";utility to dump esri shapefiles into various text formats\n");
        exit( 5 );
    }
    argFilename = argv[1];

/* -------------------------------------------------------------------- */
/*      Open the passed shapefile and the associated database           */
/* -------------------------------------------------------------------- */
    hSHP = SHPOpen( argFilename, "rb" );
    if( hSHP == NULL )
    {
	printf( "Unable to open shapefile: %s\n", argFilename );
	exit( 1 );
    }

    hDBF = DBFOpen( argv[1], "rb" );
    if( hDBF == NULL )
    {
    printf( "Unable to open shapefile database: %s\n", argFilename );
    exit( 2 );
    }

/* -------------------------------------------------------------------- */
/*      Print out the file bounds or file headers                       */
/* -------------------------------------------------------------------- */
    SHPGetInfo( hSHP, &nEntities, &nShapeType, adfMinBound, adfMaxBound );
    dFields = DBFGetFieldCount(hDBF);
    dEntities = DBFGetRecordCount(hDBF);

    if( bFormat == "gpx" )
    {
        printf( "<?xml version=\"1.0\"?>\n" );
        printf( "<gpx version=\"1.0\" creator=\"http://www.obviously.com/\" xmlns:xsi=\"http://www.w3.org/2001/XMLSchema-instance\" xmlns=\"http://www.topografix.com/GPX/1/0\" xsi:schemaLocation=\"http://www.topografix.com/GPX/1/0 http://www.topografix.com/GPX/1/0/gpx.xsd\">\n" );

        printf( "<bounds minlat=\"%12.8f\" minlon=\"%12.8f\" maxlat=\"%12.8f\" maxlon=\"%12.8f7\"/>\n",
                adfMinBound[1],adfMinBound[0],adfMaxBound[1],adfMaxBound[0] );
        printf( "<name>%s</name>\n", argFilename );
            
    }
    else if (  bFormat == "spreadsheet" )
    {
        printf( "id\tX-Coordinate\tY-Coordinate\tZ\tM");
        for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
            DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );
            printf( "\t%s", szTitle );
        }
        printf( "\n" );
    }
    else if (  bFormat == "geo.position" )
    {
    }
    else if (  bFormat == "text" )
    {
        printf( "Shapefile Type: %s   # of Shapes: %d\n\n",
                SHPTypeName( nShapeType ), nEntities );
        
        printf( "File Bounds: (%12.6f,%12.6f,%g,%g)\n"
                "         to  (%12.6f,%12.6f,%g,%g)\n",
                adfMinBound[0], 
                adfMinBound[1], 
                adfMinBound[2], 
                adfMinBound[3], 
                adfMaxBound[0], 
                adfMaxBound[1], 
                adfMaxBound[2], 
                adfMaxBound[3] );

        printf( "Database records %d, fields %d\n", dEntities, dFields);
    }

/* -------------------------------------------------------------------- */
/*  Dump header definitions.                                            */
/* -------------------------------------------------------------------- */
    if( bFormat == "text" || bFormat == "fields" )
    {
        for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
        {
            DBFFieldType    eType;
            const char      *pszTypeName;

            eType = DBFGetFieldInfo( hDBF, i, szTitle, &nWidth, &nDecimals );

            if( eType == FTString )
                pszTypeName = "String";
            else if( eType == FTInteger )
                pszTypeName = "Integer";
            else if( eType == FTDouble )
                pszTypeName = "Double";
            else if( eType == FTInvalid )
                pszTypeName = "Invalid";

            printf( "Field %d: Type=%s, Title=`%s', Width=%d, Decimals=%d\n",
                    i, pszTypeName, szTitle, nWidth, nDecimals );
        }
    }

        
/* -------------------------------------------------------------------- */
/*  Processs each shape, and associated database record                 */
/* -------------------------------------------------------------------- */
    for( iRecord = 0; iRecord < nEntities; iRecord++ )
    {
	int		j;
    SHPObject	*psShape;
    char    iRoutePart = 'a';

	psShape = SHPReadObject( hSHP, iRecord );

    /*  Print header for each new shape */
    if( bFormat == "gpx" && psShape->nVertices > 1)
    {
        printf( "\n<rte>" );
        printf( "<number>%d</number>",iRecord);
        temp1 = (char *)DBFReadStringAttribute( hDBF, iRecord, argNameField );
        temp2 = strchr(temp1,'&');      // Search for illegal xml character
        if( temp2 ) { *temp2 = '-'; }   // Convert & to -
        printf( "<name>%s</name>",    temp1);
        printf( "<cmt>%s</cmt>",      DBFReadStringAttribute( hDBF, iRecord, argAttributeField ));
        printf( "\n" );
    }
    else if (  bFormat == "geo.position" )
    {
            if( psShape->nVertices == 1 )
            {
                printf("  <meta name=\"geo.position\" content=\"");
            } else
            {
                printf("  <meta name=\"geo.polyline\" content=\"");
            }
    }
    else if (  bFormat == "spreadsheet" )
    {
    }
    else if (  bFormat == "text" )
    {
        printf( "\nShape:%d (%s)  nVertices=%d, nParts=%d\n"
                    "  Bounds:(%12.6f,%12.6f, %g, %g)\n"
                    "      to (%12.6f,%12.6f, %g, %g)\n",
                i, SHPTypeName(psShape->nSHPType),
                    psShape->nVertices, psShape->nParts,
                    psShape->dfXMin, psShape->dfYMin,
                    psShape->dfZMin, psShape->dfMMin,
                    psShape->dfXMax, psShape->dfYMax,
                    psShape->dfZMax, psShape->dfMMax );
    }

    /*  Print each sub vertex in current shape */
    for( j = 0, iPart = 1; j < psShape->nVertices; j++ )
    {
            const char	*pszPartType = "";

            if( j == 0 && psShape->nParts > 0 )
                pszPartType = SHPPartTypeName( psShape->panPartType[0] );
            
        if( iPart < psShape->nParts
                && psShape->panPartStart[iPart] == j )
        {
                pszPartType = SHPPartTypeName( psShape->panPartType[iPart] );
        iPart++;
        pszPlus = "+";

            if( bFormat == "gpx" )
            {
            temp1 = (char *)DBFReadStringAttribute( hDBF, iRecord, argNameField );
            temp2 = strchr(temp1,'&');      // Search for illegal xml character
            if( temp2 ) { *temp2 = '-'; }   // Convert & to -
            printf( "</rte>\n");
            printf( "<rte>");
            printf( "<number>%d</number>",iRecord);
            printf( "<name>%s</name>",    temp1 );
            printf( "<cmt>%s</cmt>\n",    DBFReadStringAttribute( hDBF, iRecord, argAttributeField ));
            }        
        }
        else
            pszPlus = " ";

        if( bFormat == "gpx" && psShape->nVertices > 1)
        {
            printf(" <rtept lat=\"%12.8f\" lon=\"%12.8f\"></rtept>\n", psShape->padfY[j],psShape->padfX[j]);
        }        
        else if( bFormat == "gpx" )
        {
            temp1 = (char *)DBFReadStringAttribute( hDBF, iRecord, argNameField );
            temp2 = strchr(temp1,'&');      // Search for illegal xml character
            if( temp2 ) { *temp2 = '-'; }   // Convert & to -

            printf("<wpt lat=\"%12.8f\" lon=\"%12.8f\">", psShape->padfY[j],psShape->padfX[j]);
            printf("<name>%s</name>",temp1);
            printf("<sym>%s</sym>",  DBFReadStringAttribute( hDBF, iRecord, argAttributeField ));
            printf("</wpt>\n");
        }
        else if (  bFormat == "geo.position" )
        {
            printf("%12.8f;%12.8f ", psShape->padfY[j],psShape->padfX[j]);
        }
        else if (  bFormat == "spreadsheet" )
        {
            printf("%d\t%12.8f\t%12.8f\t%g\t%g\t",
                   iRecord,psShape->padfX[j],psShape->padfY[j],psShape->padfZ[j],psShape->padfM[j]);
            for( i = 0; i < DBFGetFieldCount(hDBF); i++ )
            {
                printf( "%s\t", DBFReadStringAttribute( hDBF, iRecord, i ));
            }
            printf("\n");
        }
        else if (  bFormat == "text" )
        {
            printf("   %s (%12.8f,%12.8f, %g, %g) %s \n",
                       pszPlus,
                       psShape->padfX[j],
                       psShape->padfY[j],
                       psShape->padfZ[j],
                       psShape->padfM[j],
                       pszPartType );
        }
    }

    SHPDestroyObject( psShape );

    if( bFormat == "gpx" && psShape->nVertices > 1)
    {
        printf( "</rte>\n");
    } else if ( bFormat == "geo.position" )
    {
        printf("\" />\n");
    }
    }   /* end record */

    SHPClose( hSHP );

    if( bFormat == "gpx" )
    {
        printf( "</gpx>\n");
    }

#ifdef USE_DBMALLOC
    malloc_dump(2);
#endif

    exit( 0 );
}

