/* main.cpp - Main entry point for the tool GUI
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

#include "pages.h"
#include "tools.h"

class ScummVMToolsApp : public wxApp
{
	virtual bool OnInit();
};

IMPLEMENT_APP(ScummVMToolsApp)

bool ScummVMToolsApp::OnInit()
{
	// Init tools
	g_tools.init();

	// Create window & display
	ScummToolsFrame *frame = new ScummToolsFrame(wxT("ScummVM Tools"), wxDefaultPosition, wxSize(600,400));
	frame->SetMinSize(wxSize(600, 400));
	frame->Show(true);
	SetTopWindow(frame);

	return true;
}

BEGIN_EVENT_TABLE(ScummToolsFrame, wxFrame)
	EVT_IDLE(ScummToolsFrame::onIdle)
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
	
	// Create input page
	WizardPage *introPage = new IntroPage(this);

	// We create the intro page once the window is setup
	wxSizer *panesizer = new wxBoxSizer(wxVERTICAL);
	wxWindow *introPanel = introPage->CreatePanel(_wizardpane);
	panesizer->Add(introPanel, wxSizerFlags(1).Expand());
	_wizardpane->SetSizerAndFit(panesizer);

	main->SetSizer(sizer);

	// Set current page
	_pages.push_back(introPage);

	// And reset the buttons to a standard state
	_buttons->setPage(introPage, introPanel);
}

ScummToolsFrame::~ScummToolsFrame() {
	for (std::vector<WizardPage *>::iterator iter = _pages.begin(); iter != _pages.end(); ++iter)
		delete *iter;
}

void ScummToolsFrame::switchPage(WizardPage *next, bool moveback) {
	// Find the old page
	wxPanel *oldPanel = dynamic_cast<wxPanel *>(_wizardpane->FindWindow(wxT("Wizard Page")));

	_pages.back()->save(oldPanel);

	if (moveback) {
		// Don't save the old page (which is ontop of the stack already)
		delete _pages.back();
		_pages.pop_back();
	} else {
		_pages.push_back(next);
	}

	// Destroy the old page
	oldPanel->Destroy();

	wxWindow *newPanel = _pages.back()->CreatePanel(_wizardpane);

	// Add the new page!
	_wizardpane->GetSizer()->Add(newPanel, wxSizerFlags(1).Expand());

	// Make sure it fits
	_wizardpane->Layout();

	// And reset the buttons to a standard state
	_buttons->reset();
	_buttons->setPage(_pages.back(), newPanel);
}

void ScummToolsFrame::onIdle(wxIdleEvent &evt) {
	if (_pages.back()->onIdle(dynamic_cast<wxPanel *>(_wizardpane->FindWindow(wxT("Wizard Page"))))) {
		// We want more!
		evt.RequestMore(true);
	}
}

//

BEGIN_EVENT_TABLE(WizardButtons, wxPanel)
	EVT_BUTTON(ID_NEXT, WizardButtons::onClickNext)
	EVT_BUTTON(ID_PREV, WizardButtons::onClickPrevious)
	EVT_BUTTON(ID_CANCEL, WizardButtons::onClickCancel)
END_EVENT_TABLE()

WizardButtons::WizardButtons(wxWindow *parent, wxStaticText *linetext)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("WizardButtonPanel")),
	  _linetext(linetext),
	  _currentPage(NULL)
{
	wxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);

	_prev = new wxButton(this, ID_PREV, wxT("< Back"));
	_prev->SetSize(80, -1);
	sizer->Add(_prev, wxSizerFlags().Center().ReserveSpaceEvenIfHidden());

	_next = new wxButton(this, ID_NEXT, wxT("Next >"));
	_next->SetSize(80, -1);
	sizer->Add(_next, wxSizerFlags().Center().ReserveSpaceEvenIfHidden());

	sizer->AddSpacer(10);

	_cancel = new wxButton(this, ID_CANCEL, wxT("Cancel"));
	_cancel->SetSize(80, -1);
	sizer->Add(_cancel, wxSizerFlags().Center().ReserveSpaceEvenIfHidden());

	SetSizerAndFit(sizer);

	reset();
}

void WizardButtons::reset() {
	enableNext(true);
	enablePrevious(true);
	showFinish(false);
	setLineLabel(wxT("ScummVM Tools"));
}

void WizardButtons::setPage(WizardPage *current, wxWindow *panel) {
	_currentPage = current;
	_currentPanel = panel;
	// We call onUpdateButtons, which sets the _buttons member of WizardPage
	// to this, and in turn calls updateButtons on itself and sets up the buttons
	// We cannot set this up in the constructor of the WizardPage, since it's impossible
	// to call reset *before* the page is created from SwicthPage
	_currentPage->updateButtons(_currentPanel, this);
}

void WizardButtons::setLineLabel(wxString label) {
	_linetext->SetLabel(label);
}

void WizardButtons::enableNext(bool enable) {
	_next->Enable(enable);
}

void WizardButtons::enablePrevious(bool enable) {
	if (enable)
		showPrevious(true);
	_prev->Enable(enable);
}

void WizardButtons::showFinish(bool show) {
	if (show)
		_next->SetLabel(wxT("Finish!"));
	else
		_next->SetLabel(wxT("Next >"));
}

void WizardButtons::showPrevious(bool show) {
	if (show)
		_prev->Show();
	else
		_prev->Hide();
}

// wx event handlers
void WizardButtons::onClickNext(wxCommandEvent &e) {
	wxASSERT(_currentPage);
	_currentPage->onNext(_currentPanel);
}

void WizardButtons::onClickPrevious(wxCommandEvent &e) {
	wxASSERT(_currentPage);
	_currentPage->onPrevious(_currentPanel);
}

void WizardButtons::onClickCancel(wxCommandEvent &e) {
	wxASSERT(_currentPage);
	_currentPage->onCancel(_currentPanel);
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
	if (wxImage::FindHandler(wxT("jpg")) == NULL)
		wxImage::AddHandler(new wxJPEGHandler);
	if (wxImage::FindHandler(wxT("gif")) == NULL)
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
	
	if (_logo.IsOk() == false || _tile.IsOk() == false) {
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

		while (x < w) {
			dc.DrawBitmap(_tile, x, 0);
			x += _tile.GetWidth();
		}
	}
	
	dc.SetFont(_font);
	dc.DrawText(wxT("Extraction & Compression Wizard"), 290, 70);
}

