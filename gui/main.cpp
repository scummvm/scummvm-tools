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
	WizardPage *introPage = new IntroPage(_wizardpane);
	panesizer->Add(introPage, wxSizerFlags(1).Expand());
	_wizardpane->SetSizerAndFit(panesizer);


	main->SetSizer(sizer);

	// And reset the buttons to a standard state
	_buttons->setPage(introPage);
}

ScummToolsFrame::~ScummToolsFrame() {
	for(std::vector<WizardPageClass *>::iterator iter = _previousPages.begin(); iter != _previousPages.end(); ++iter)
		delete *iter;
}

void ScummToolsFrame::SwitchPage(WizardPage *next) {
	// Find the old page
	WizardPage *old = dynamic_cast<WizardPage*>(_wizardpane->FindWindow(wxT("Wizard Page")));

	wxASSERT_MSG(old, wxT("Expected the child 'Wizard Page' to be an actual Wizard Page window."));

	old->save(configuration);

	// Save the old page
	_previousPages.push_back(old->getClass());

	// Destroy the old page
	old->Destroy();

	next->load(configuration);

	// Add the new page!
	next->Reparent(_wizardpane);
	_wizardpane->GetSizer()->Add(next, wxSizerFlags(1).Expand());

	// Make sure it fits
	_wizardpane->Fit();

	// And reset the buttons to a standard state
	_buttons->reset();
	_buttons->setPage(next);
}

void ScummToolsFrame::SwitchToPreviousPage() {
	WizardPage *prev = _previousPages.back()->create(_wizardpane);
	delete _previousPages.back();
	_previousPages.pop_back();
	SwitchPage(prev);
}

BEGIN_EVENT_TABLE(WizardButtons, wxPanel)
	EVT_BUTTON(ID_NEXT, WizardButtons::onClickNext)
	EVT_BUTTON(ID_PREV, WizardButtons::onClickPrevious)
	EVT_BUTTON(ID_CANCEL, WizardButtons::onClickCancel)
END_EVENT_TABLE()

WizardButtons::WizardButtons(wxWindow *parent, wxStaticText *linetext)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Button Panel")),
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
}

void WizardButtons::setPage(WizardPage *current) {
	_currentPage = current;
	// We call onUpdateButtons, which sets the _buttons member of WizardPage
	// to this, and in turn calls updateButtons on itself and sets up the buttons
	// We cannot set this up in the constructor of the WizardPage, since it's impossible
	// to call reset *before* the page is created from SwicthPage
	current->onUpdateButtons(this);
}

void WizardButtons::enableNext(bool enable) {
	_next->Enable(enable);
}

void WizardButtons::enablePrevious(bool enable) {
	if(enable)
		showPrevious(true);
	_prev->Enable(enable);
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

// wx event handlers
void WizardButtons::onClickNext(wxCommandEvent &e) {
	wxASSERT(_currentPage);
	_currentPage->Hide();
	_currentPage->onNext();
}

void WizardButtons::onClickPrevious(wxCommandEvent &e) {
	wxASSERT(_currentPage);
	_currentPage->onPrevious();
}

void WizardButtons::onClickCancel(wxCommandEvent &e) {
	wxASSERT(_currentPage);
	_currentPage->onCancel();
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

