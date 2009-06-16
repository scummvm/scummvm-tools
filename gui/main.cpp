/* gui_main.cpp - Main entry point for the tool GUI
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

#include "wx/wxprec.h"

#ifdef __BORLANDC__
	#pragma hdrstop
#endif

#ifndef WX_PRECOMP
	#include "wx/wx.h"
#endif

#include <wx/statline.h>

#include "main.h"

class ScummVMToolsApp : public wxApp
{
	virtual bool OnInit();
};

IMPLEMENT_APP(ScummVMToolsApp)

bool ScummVMToolsApp::OnInit()
{
	ScummToolsFrame *frame = new ScummToolsFrame(wxT("ScummVM Tools"), wxDefaultPosition, wxSize(600,400));
	frame->SetMinSize(wxSize(600, 400));
	frame->Show(true);
	SetTopWindow(frame);
	return true;
}

BEGIN_EVENT_TABLE(ScummToolsFrame, wxFrame)
END_EVENT_TABLE()

ScummToolsFrame::ScummToolsFrame(const wxString &title, const wxPoint &pos, const wxSize& size)
		: wxFrame((wxFrame *)NULL, -1, title, pos, size)
{
	// We need a parent frame for correct background color (default frame looks 'disabled' in the background)
	wxPanel *main = new wxPanel(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Main Panel"));


	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);
	
	// Add the top header, it's sweet!
	sizer->Add(new Header(main), wxSizerFlags(0).Expand());

	// Pane that holds the wizard window
	_wizardpane = new wxPanel(main);
	sizer->Add(_wizardpane, wxSizerFlags(1).Expand().Border());

	// Add a spacer line
	// We split it in two parts over a panel to have a small text there
	wxPanel *linepanel = new wxPanel(main, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Line Panel"));
	wxSizer *linesizer = new wxBoxSizer(wxHORIZONTAL);
	
	wxStaticText *linetext = new wxStaticText(linepanel, wxID_ANY, wxT("ScummVM Tools"));
	linesizer->Add(linetext, wxSizerFlags());
	linetext->Disable();

	wxStaticLine *line = new wxStaticLine(
			linepanel, wxID_ANY, 
			wxDefaultPosition, wxSize(300, 1), 
			wxBORDER_SIMPLE | wxLI_HORIZONTAL, wxT("Line Spacer")
		);
	line->Disable();
	linesizer->Add(line, wxSizerFlags(1).Center());

	linepanel->SetSizer(linesizer);
	
	// Add the line to the main panel
	sizer->Add(linepanel, wxSizerFlags().Expand().Center().Border());

	// Buttons on the bottom
	_buttons = new WizardButtons(main, linetext);
	sizer->Add(_buttons, wxSizerFlags().Border().Right());
	
	// We create the intro page once the window is setup
	wxSizer *panesizer = new wxBoxSizer(wxVERTICAL);
	panesizer->Add(new IntroPage(_wizardpane, _buttons), wxSizerFlags(1).Expand());
	_wizardpane->SetSizerAndFit(panesizer);


	main->SetSizer(sizer);
}

void ScummToolsFrame::SwitchPage(WizardPage *nextPage) {
	
}

WizardButtons::WizardButtons(wxWindow *parent, wxStaticText *linetext)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Button Panel")),
	  _linetext(linetext)
{
	wxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);

	_prev = new wxButton(this, wxID_ANY, wxT("< Back"));
	_prev->SetSize(80, -1);
	sizer->Add(_prev, wxSizerFlags().Center().ReserveSpaceEvenIfHidden());

	_next = new wxButton(this, wxID_ANY, wxT("Next >"));
	_next->SetSize(80, -1);
	sizer->Add(_next, wxSizerFlags().Center().ReserveSpaceEvenIfHidden());

	sizer->AddSpacer(10);

	_cancel = new wxButton(this, wxID_ANY, wxT("Cancel"));
	_cancel->SetSize(80, -1);
	sizer->Add(_cancel, wxSizerFlags().Center().ReserveSpaceEvenIfHidden());

	SetSizerAndFit(sizer);
}

void WizardButtons::enableNext(bool enable) {
	_next->Enable(enable);
}

void WizardButtons::enablePrevious(bool enable) {
	_next->Enable(enable);
}

void WizardButtons::showFinish(bool show) {
	if(show)
		_next->SetLabel(wxT("Finish!"));
	else
		_next->SetLabel(wxT("Next >"));
}

void WizardButtons::showPrevious(bool show) {
	if(show)
		_prev->Show();
	else
		_prev->Hide();
}

BEGIN_EVENT_TABLE(Header, wxPanel)
	EVT_PAINT(Header::onPaint)
END_EVENT_TABLE()

Header::Header(wxWindow *parent)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxSize(400, 118), wxBORDER_NONE, wxT("Wizard Splash"))
{
	// Disable warnings in this function
	wxLogNull nulllog;

	// Add support for loading .jpg images
	if(wxImage::FindHandler(wxT("jpg")) == NULL)
		wxImage::AddHandler(new wxJPEGHandler);
	if(wxImage::FindHandler(wxT("gif")) == NULL)
		wxImage::AddHandler(new wxGIFHandler);

	// Load image files
	_logo.LoadFile(wxT("media/logo.jpg"), wxBITMAP_TYPE_JPEG);
	_tile.LoadFile(wxT("media/tile.gif"), wxBITMAP_TYPE_GIF);

	// Load font
	_font = wxFont(10, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false);
}

void Header::onPaint(wxPaintEvent &evt) {
	wxPaintDC dc(this);

	int w, h;
	this->GetSize(&w, &h);
	
	if(_logo.IsOk() == false || _tile.IsOk() == false) {
		// If we couldn't load the images, use orange instead!
		dc.SetBackground(wxBrush(wxColor(213, 114, 0)));
		dc.Clear();

		// Draw lighter stripe below, looks good
		dc.SetPen(wxPen(*wxBLACK, 0, wxTRANSPARENT));
		dc.SetBrush(wxBrush(wxColor(245, 228, 156)));
		dc.DrawRectangle(0, 90, w, h - 90);
	} else {
		// We got some good-looking images! Draw them!
		int x = 0;
		dc.DrawBitmap(_logo, x, 0);
		x += _logo.GetWidth();

		while(x < w) {
			dc.DrawBitmap(_tile, x, 0);
			x += _tile.GetWidth();
		}
	}
	
	dc.SetFont(_font);
	dc.DrawText(wxT("Extraction & Compression Wizard"), 290, 70);
}


WizardPage::WizardPage(wxWindow *parent, WizardButtons *buttons)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Page")),
	  _nextPage(NULL),
	  _prevPage(NULL),
	  _buttons(buttons)
{
}

void WizardPage::SetAlignedSizer(wxSizer *sizer) {
	wxSizer *topsizer = new wxBoxSizer(wxHORIZONTAL);
	topsizer->AddSpacer(100);
	topsizer->Add(sizer);
	SetSizer(topsizer);
}

IntroPage::IntroPage(wxWindow *parent, WizardButtons *buttons)
	: WizardPage(parent, buttons)
{
	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(this, wxID_ANY, 
		wxT("Welcome to the ScummVM extraction and compression utility.")));
	sizer->Add(new wxStaticText(this, wxID_ANY,
		wxT("Please select what you want to do, or drop a file or folder on this window for automatic .")));
	
	wxString choices[] = {
		wxT("Extract from game data files"),
		wxT("Compress audio files")
	};

	_options = new wxRadioBox(this, wxID_ANY, wxT(""), wxDefaultPosition, wxDefaultSize, 2, choices, 1, wxRA_SPECIFY_COLS | wxBORDER_NONE);
	sizer->Add(_options);
	_options->SetSelection(0);

	SetAlignedSizer(sizer);

	_buttons->showPrevious(false);
	_buttons->enableNext(true);
}

