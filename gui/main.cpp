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
#include <wx/aboutdlg.h>
#include "wx/stdpaths.h"

#include "main.h"

#include "pages.h"
#include "gui_tools.h"


/**
 * The application class, main entry point for wxWidgets applications.
 */
class ScummVMToolsApp : public wxApp {
	virtual bool OnInit();

public:
	void OnAbout();
};

IMPLEMENT_APP(ScummVMToolsApp)

bool ScummVMToolsApp::OnInit() {
	// Init tools
	g_tools.init();
	
	SetAppName(wxT("ScummVM Tools"));

	// Create window & display
	ScummToolsFrame *frame = new ScummToolsFrame(GetAppName(), wxDefaultPosition, wxSize(600,400));
#ifdef __WXMAC__ // Menu bar looks ugly when it's part of the window, on OSX it's not
	frame->CreateMenuBar();
#endif
	frame->SetMinSize(wxSize(600, 400));
	SetTopWindow(frame);
	
	if (argc == 2) {
		Filename fn((const char *)wxString(argv[1]).mb_str());

		wxArrayString ls = g_tools.getToolList(fn);
		if(ls.size() == 1)
			frame->switchPage(new ChooseOutPage(frame));
		else
			frame->switchPage(new ChooseToolPage(frame, ls));
	} else {
		frame->switchPage(new IntroPage(frame));
	}
	
	frame->Show(true);
	
	return true;
}

void ScummVMToolsApp::OnAbout() {
	wxAboutDialogInfo about = wxAboutDialogInfo();
	about.SetVersion(wxT("Development Version"));
	about.SetCopyright(wxT("ScummVM Team 2009"));
	about.SetWebSite(wxT("http://www.scummvm.org"));
	about.SetLicense(
		wxT("Published under the GNU General Public License\n")
		wxT("This program comes with ABSOLUTELY NO WARRANTY\n")
		wxT("This is free software, and you are welcome to redistribute it ")
		wxT("under certain conditions"));
	about.SetDescription(
		wxT("This tool allows you to extract data files from several different games \n")
		wxT("to be used by ScummVM, it can also compress audio data files into a more \n")
		wxT("compact format than the original."));
	::wxAboutBox(about);
}


// Event table for the top frame
BEGIN_EVENT_TABLE(ScummToolsFrame, wxFrame)
	//EVT_MENU(wxID_PREFERENCES, ScummToolsFrame::onMenuPreferences)
	EVT_BUTTON(ID_HELP, ScummToolsFrame::onMenuHelp)
	EVT_MENU(wxID_HELP, ScummToolsFrame::onMenuHelp)
	EVT_MENU(wxID_ABOUT, ScummToolsFrame::onMenuAbout)
	EVT_BUTTON(ID_ABOUT, ScummToolsFrame::onMenuAbout)
	EVT_MENU(wxID_EXIT, ScummToolsFrame::onMenuExit)

	EVT_IDLE(ScummToolsFrame::onIdle)
	EVT_CLOSE(ScummToolsFrame::onClose)
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
	_wizardpane->SetSizer(new wxBoxSizer(wxVERTICAL));
	sizer->Add(_wizardpane, wxSizerFlags(1).Expand().Border());

	// Add a spacer line
	// We split it in two parts over a panel to have a small text there
	wxPanel *linepanel = new wxPanel(main, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Line Panel"));
	wxSizer *linesizer = new wxBoxSizer(wxHORIZONTAL);
	
	wxStaticText *linetext = new wxStaticText(linepanel, wxID_ANY, wxGetApp().GetAppName());
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
	_buttons = new WizardButtons(main, linetext, _configuration);
	sizer->Add(_buttons, wxSizerFlags().Border().Center().Expand());

	main->SetSizer(sizer);
}

ScummToolsFrame::~ScummToolsFrame() {
	for (std::vector<WizardPage *>::iterator iter = _pages.begin(); iter != _pages.end(); ++iter)
		delete *iter;
}

void ScummToolsFrame::CreateMenuBar() {
	wxMenuBar *menubar = new wxMenuBar();

#ifdef __WXMAC__
	wxApp::s_macHelpMenuTitleName = "Help";
#endif 

	// Name of this seems really inappropriate
	wxMenu *helpmenu = new wxMenu();
	//filemenu->Append(wxID_PREFERENCES, wxT("&Preferences"));
	helpmenu->Append(wxID_HELP, wxT("Help"));
	helpmenu->Append(wxID_ABOUT, wxT("&About ") + wxGetApp().GetAppName());
	menubar->Append(helpmenu, wxT("Help"));

	SetMenuBar(menubar);
}

void ScummToolsFrame::switchPage(WizardPage *next, bool moveback) {
	// Find the old page
	wxPanel *oldPanel = dynamic_cast<wxPanel *>(_wizardpane->FindWindow(wxT("Wizard Page")));

	if (oldPanel)
		_pages.back()->save(oldPanel);

	if (moveback) {
		// Don't save the old page (which is ontop of the stack already)
		delete _pages.back();
		_pages.pop_back();
	} else {
		_pages.push_back(next);
	}

	if (oldPanel)
		// Destroy the old page
		oldPanel->Destroy();

	wxWindow *newPanel = _pages.back()->CreatePanel(_wizardpane);

	// Add the new page!
	_wizardpane->GetSizer()->Add(newPanel, wxSizerFlags(1).Expand());

	// Make sure it fits
	_wizardpane->Layout();

	// And reset the buttons to a standard state
	_buttons->reset();
	_buttons->showPrevious(_pages.size() > 1);
	_buttons->setPage(_pages.back(), newPanel);
}

wxString ScummToolsFrame::GetHelpText() {
	return _pages.back()->getHelp();
}

void ScummToolsFrame::onMenuHelp(wxCommandEvent &evt) {
	wxString help = _pages.back()->getHelp();
	wxMessageDialog dlg(this, help, wxT("Help"));
	dlg.ShowModal();
}

void ScummToolsFrame::onMenuAbout(wxCommandEvent &evt) {
	wxGetApp().OnAbout();
}

void ScummToolsFrame::onMenuExit(wxCommandEvent &evt) {
	Close();
}

void ScummToolsFrame::onIdle(wxIdleEvent &evt) {
	if (_pages.back()->onIdle(dynamic_cast<wxPanel *>(_wizardpane->FindWindow(wxT("Wizard Page"))))) {
		// We want more!
		evt.RequestMore(true);
		// This way we don't freeze the OS with continous events
		wxMilliSleep(10);
	}
}

void ScummToolsFrame::onClose(wxCloseEvent &evt) {
	if (!evt.CanVeto() || _pages.back()->onCancel(dynamic_cast<wxPanel *>(_wizardpane->FindWindow(wxT("Wizard Page")))))
		wxFrame::OnCloseWindow(evt);
}


// Event table for the WizardButtons window
BEGIN_EVENT_TABLE(WizardButtons, wxPanel)
	//EVT_BUTTON(ID_HELP, WizardButtons::onClickHelp)
	//EVT_BUTTON(ID_ABOUT, WizardButtons::onClickAbout)
	EVT_BUTTON(ID_NEXT, WizardButtons::onClickNext)
	EVT_BUTTON(ID_PREV, WizardButtons::onClickPrevious)
	EVT_BUTTON(ID_CANCEL, WizardButtons::onClickCancel)
END_EVENT_TABLE()

WizardButtons::WizardButtons(wxWindow *parent, wxStaticText *linetext, Configuration &conf)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("WizardButtonPanel")),
	  _configuration(conf),
	  _linetext(linetext),
	  _currentPage(NULL)
{
	wxSizer *topsizer = new wxBoxSizer(wxHORIZONTAL);

	wxSizer *sizer = new wxBoxSizer(wxHORIZONTAL);

	_prev = new wxButton(this, ID_ABOUT, wxT("About"));
	_prev->SetSize(80, -1);
	sizer->Add(_prev, wxSizerFlags().Left().ReserveSpaceEvenIfHidden());

	sizer->AddSpacer(10);

	_help = new wxButton(this, ID_HELP, wxT("Help"));
	_help->SetSize(80, -1);
	sizer->Add(_help, wxSizerFlags().Left().ReserveSpaceEvenIfHidden());

	// Insert space between the buttons
	topsizer->Add(sizer, wxSizerFlags().Left());
	topsizer->Add(10, 10, 1, wxEXPAND);
	sizer = new wxBoxSizer(wxHORIZONTAL);


	_prev = new wxButton(this, ID_PREV, wxT("< Back"));
	_prev->SetSize(80, -1);
	sizer->Add(_prev, wxSizerFlags().Right().ReserveSpaceEvenIfHidden());

	_next = new wxButton(this, ID_NEXT, wxT("Next >"));
	_next->SetSize(80, -1);
	sizer->Add(_next, wxSizerFlags().Right().ReserveSpaceEvenIfHidden());

	sizer->AddSpacer(10);

	_cancel = new wxButton(this, ID_CANCEL, wxT("Cancel"));
	_cancel->SetSize(80, -1);
	sizer->Add(_cancel, wxSizerFlags().Right().ReserveSpaceEvenIfHidden());
	
	topsizer->Add(sizer, wxSizerFlags().Right());

	SetSizerAndFit(topsizer);

	reset();
}

void WizardButtons::reset() {
	enableNext(true);
	enablePrevious(true);
	showFinish(false);
	showAbort(false);
	showNavigation(true);

	wxString label(wxGetApp().GetAppName());
	if (_configuration.selectedTool)
		label << wxT(" - ") << _configuration.selectedTool->getName();
	setLineLabel(label);
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
	_prev->Enable(enable);
}

void WizardButtons::showFinish(bool show) {
	if (show)
		_next->SetLabel(wxT("Finish!"));
	else
		_next->SetLabel(wxT("Next >"));
}

void WizardButtons::showAbort(bool show) {
	if (show)
		_cancel->SetLabel(wxT("Abort"));
	else
		_cancel->SetLabel(wxT("Cancel"));
}

void WizardButtons::showNavigation(bool show) {
	if (show) {
		_next->Show();
		_prev->Show();
	} else {
		_next->Hide();
		_prev->Hide();
	}
}

void WizardButtons::showPrevious(bool show) {
	if (show) {
		_prev->Show();
	} else {
		_prev->Hide();
	}
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
#ifdef __WXMAC__
	_logo.LoadFile(wxStandardPaths::Get().GetResourcesDir() + wxT("/logo.jpg"), wxBITMAP_TYPE_JPEG);
	_tile.LoadFile(wxStandardPaths::Get().GetResourcesDir() + wxT("/tile.gif"), wxBITMAP_TYPE_GIF);
#else
	_logo.LoadFile(wxT("media/logo.jpg"), wxBITMAP_TYPE_JPEG);
	_tile.LoadFile(wxT("media/tile.gif"), wxBITMAP_TYPE_GIF);
#endif

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

