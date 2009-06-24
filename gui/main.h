/* main.h - Main window classes for the tool GUI
 * Copyright (C) 2009 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.

 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.

 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 *
 * $URL$
 * $Id$
 *
 */

#ifndef GUI_MAIN_H
#define GUI_MAIN_H

#include <wx/wx.h>
#include <vector>

#include "configuration.h"

class WizardButtons;
struct WizardPageClass;
class WizardPage;

enum GUI_ID {
	ID_FIRST = wxID_HIGHEST, // Ensure no collisions
	ID_NEXT,
	ID_PREV,
	ID_CANCEL,
};

// Application top window

class ScummToolsFrame : public wxFrame
{
public:
	ScummToolsFrame(const wxString &title, const wxPoint &pos, const wxSize &size);
	~ScummToolsFrame();

	// Switches to this page and resets the buttons
	void switchPage(WizardPage *nextPage, bool moveback = false);

	// Switches to the previous page
	void switchToPreviousPage();

	Configuration _configuration;

private:
	wxPanel *_wizardpane;
	WizardButtons *_buttons;
	
	std::vector<WizardPage *> _pages;

	DECLARE_EVENT_TABLE()
};

// This panel contains the "Next", "Previous" and "Abort" buttons
// Convenient interface as you don't need to create buttons
// with long, awkward calls

class WizardButtons : public wxPanel {
public:
	WizardButtons(wxWindow *parent, wxStaticText *linetext);

	// Set the buttons to the standard configuration
	// (prev, next shown and enabled, finish disabled)
	void reset();

	// Set the current wizard page, done from SwitchPage required
	// for the buttons to know where to drop their events
	void setPage(WizardPage *current, wxWindow *panel);

	// Set the label of the line above the buttons, can display some useful info here
	void setLineLabel(wxString label);

	// Enables (ie. makes clickable) the next button (or finish for the last page)
	void enableNext(bool enable);
	void enableFinish(bool enable) {enableNext(enable);}
	// Enables the previous button, shows it if hidden
	void enablePrevious(bool enable);
	// Display the previous button, it is invisible by default
	void showPrevious(bool show);
	// Changes 'next' into 'finish'
	void showFinish(bool show);

	// wx event handlers
	// overload the virtual functions below for the page-specific handlers
	void onClickNext(wxCommandEvent &e);
	void onClickPrevious(wxCommandEvent &e);
	void onClickCancel(wxCommandEvent &e);

protected:
	wxButton *_next;
	wxButton *_prev;
	wxButton *_cancel;
	wxStaticText *_linetext;
	WizardPage *_currentPage;
	wxWindow *_currentPanel;

	DECLARE_EVENT_TABLE()
};

// The header at the top of the window
// Tries to load media/logo.gif and media/tiled.gif
// If unavailable, it will simply be orange/brigt orange

class Header : public wxPanel
{
public:
	Header(wxWindow *parent);

	void onPaint(wxPaintEvent &evt);
protected:
	wxFont _font;
	wxBitmap _logo;
	wxBitmap _tile;

	DECLARE_EVENT_TABLE()
};

#endif
