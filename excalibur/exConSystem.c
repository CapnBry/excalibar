/*
 * Copyright 2002 the Excalibur contributors (http://excalibar.sourceforge.net/)
 *
 * Portions of this software are based on the work of Slicer/Hackersquest.
 * Those portions, Copyright 2001 Slicer/Hackersquest <slicer@hackersquest.org)
 * 
 * This file is part of Excalibur.
 *
 * Excalibur is free software; you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation; either version 2 of the License, or
 * (at your option) any later version.
 *
 * Excalibur is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 */

#include "exConSystem.h"

const CON_RANGE_DEFINITION CON_RANGES[] = {
    { 0,  0,  0,  0,  0,  0},   /* Level  0 */
    {-1, -1,  0,  1,  2,  3},   /* Level  1 */
    {-1,  0,  1,  2,  3,  4},   /* Level  2 */
    { 0,  1,  2,  3,  4,  5},   /* Level  3 */
    { 1,  2,  3,  4,  5,  6},   /* Level  4 */
    { 2,  3,  4,  5,  6,  7},   /* Level  5 */
    { 3,  4,  5,  6,  7,  8},   /* Level  6 */
    { 4,  5,  6,  7,  8,  9},   /* Level  7 */
    { 5,  6,  7,  8,  9, 10},   /* Level  8 */
    { 6,  7,  8,  9, 10, 11},   /* Level  9 */
    { 6,  7,  9, 10, 11, 13},   /* Level 10 */
    { 6,  7,  9, 11, 13, 15},   /* Level 11 */
    { 6,  8, 10, 12, 14, 16},   /* Level 12 */
    { 7,  9, 11, 13, 15, 17},   /* Level 13 */
    { 8, 10, 12, 14, 16, 18},   /* Level 14 */
    { 9, 11, 13, 15, 17, 19},   /* Level 15 */
    {10, 12, 14, 16, 18, 20},   /* Level 16 */
    {11, 13, 15, 17, 19, 21},   /* Level 17 */
    {12, 14, 16, 18, 20, 22},   /* Level 18 */
    {13, 15, 17, 19, 21, 23},   /* Level 19 */
    {13, 15, 18, 20, 22, 25},   /* Level 20 */
    {13, 15, 18, 21, 24, 27},   /* Level 21 */
    {13, 16, 19, 22, 25, 28},   /* Level 22 */
    {14, 17, 20, 23, 26, 29},   /* Level 23 */
    {15, 18, 21, 24, 27, 30},   /* Level 24 */
    {16, 19, 22, 25, 28, 31},   /* Level 25 */
    {17, 20, 23, 26, 29, 32},   /* Level 26 */
    {18, 21, 24, 27, 30, 33},   /* Level 27 */
    {19, 22, 25, 28, 31, 34},   /* Level 28 */
    {20, 23, 26, 29, 32, 35},   /* Level 29 */
    {21, 24, 27, 30, 33, 36},   /* Level 30 */
    {22, 25, 28, 31, 34, 37},   /* Level 31 */
    {23, 26, 29, 32, 35, 38},   /* Level 32 */
    {24, 27, 30, 33, 36, 39},   /* Level 33 */
    {25, 28, 31, 34, 37, 40},   /* Level 34 */
    {25, 28, 31, 35, 39, 42},   /* Level 35 */
    {25, 28, 31, 36, 41, 45},   /* Level 36 */
    {25, 29, 32, 37, 42, 47},   /* Level 37 */
    {25, 29, 33, 38, 43, 48},   /* Level 38 */
    {25, 29, 34, 39, 44, 49},   /* Level 39 */
    {25, 30, 35, 40, 45, 50},   /* Level 40 */
    {26, 31, 36, 41, 46, 51},   /* Level 41 */
    {27, 32, 37, 42, 47, 52},   /* Level 42 */
    {28, 33, 38, 43, 48, 53},   /* Level 43 */
    {29, 34, 39, 44, 49, 54},   /* Level 44 */
    {30, 35, 40, 45, 50, 55},   /* Level 45 */
    {31, 36, 41, 46, 51, 56},   /* Level 46 */
    {32, 37, 42, 47, 52, 57},   /* Level 47 */
    {33, 38, 43, 48, 53, 58},   /* Level 48 */
    {34, 39, 44, 49, 54, 59},   /* Level 49 */
    {35, 40, 45, 50, 55, 60}    /* Level 50 */
};

CONCOLOR concolor_for_level(int AObserverLvl, int ATargetLvl)
{
    const CON_RANGE_DEFINITION *con_def;

    if ((AObserverLvl < 0) || (AObserverLvl > 50))
        return ccGray;

    con_def = &CON_RANGES[AObserverLvl];

    if (ATargetLvl <= con_def->gray_max)
        return ccGray;
    if (ATargetLvl <= con_def->green_max)
        return ccGreen;
    if (ATargetLvl <= con_def->blue_max)
        return ccBlue;
    if (ATargetLvl <= con_def->yellow_max)
        return ccYellow;
    if (ATargetLvl <= con_def->orange_max)
        return ccOrange;
    if (ATargetLvl <= con_def->red_max)
        return ccRed;
    return ccPurple;
}
