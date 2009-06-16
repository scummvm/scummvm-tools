/* gui_main.h - Main window classes for the tool GUI
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

class WizardButtons;
class WizardPage;

// Application top window

class ScummToolsFrame : public wxFrame
{
public:
	ScummToolsFrame(const wxString &title, const wxPoint &pos, const wxSize &size);

	// Switches to this page and resets the buttons
	void SwitchPage(WizardPage *nextPage);

private:
	wxPanel *_wizardpane;
	WizardButtons *_buttons;

	DECLARE_EVENT_TABLE()
};

// This panel contains the "Next", "Previous" and "Abort" buttons
// Convenient interface as you don't need to create buttons
// with long, awkward calls

class WizardButtons : public wxPanel {
public:
	WizardButtons(wxWindow *parent, wxStaticText *linetext);

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

protected:
	wxButton *_next;
	wxButton *_prev;
	wxButton *_cancel;
	wxStaticText *_linetext;
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

// Wizard Page
// A page in the extraction wizard
// This class is responsible for almost everything!

class WizardPage : public wxPanel
{
public:
	WizardPage(wxWindow *parent, WizardButtons *buttons);

	// This adds an offset (about 100px) to the left of the sizer
	// to center the text somewhat, before adding it to the panel
	void SetAlignedSizer(wxSizer *sizer);

protected:
	WizardPage *_nextPage;
	WizardPage *_prevPage;
	WizardButtons *_buttons;
};

// Introduction page, with options to extract/compress

class IntroPage : public WizardPage
{
public:
	IntroPage(wxWindow *parent, WizardButtons *buttons);

protected:
	wxRadioBox *_options;
};

class TestPage : public WizardPage
{
public:
	TestPage(wxWindow *parent, WizardButtons *buttons);
};


#endif
