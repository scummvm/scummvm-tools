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
#include <wx/stdpaths.h>
#include <wx/hyperlink.h>
#include <wx/notebook.h>

#include "main.h"

#include "pages.h"
#include "gui_tools.h"


/**
 * The application class, main entry point for wxWidgets applications.
 */
class ScummVMToolsApp : public wxApp {
	virtual bool OnInit();

public:
	/**
	 * Essentially a global function to display the about dialog
	 * We keep it here since there is no good place to stick it since it's called
	 * from many different classes.
	 */
	void OnAbout();
};

IMPLEMENT_APP(ScummVMToolsApp)

bool ScummVMToolsApp::OnInit() {
	// Init tools
	g_tools.init();

	SetAppName(wxT("ScummVM Tools"));

	// Create window & display
	ScummToolsFrame *frame = new ScummToolsFrame(GetAppName(), wxDefaultPosition, wxSize(600,420));
#ifdef __WXMAC__ // Menu bar looks ugly when it's part of the window, on OSX it's not
	frame->CreateMenuBar();
	frame->CentreOnScreen();
#endif
	frame->SetMinSize(wxSize(600, 420));
	SetTopWindow(frame);

	// Create and load configuration
	Configuration &configuration = frame->_configuration;

	if (argc == 2) {
		Common::Filename fn((const char *)wxString(argv[1]).mb_str());

		wxArrayString ls = g_tools.getToolList(fn);
		if (ls.size() == 1)
			frame->switchPage(new ChooseOutPage(configuration));
		else
			frame->switchPage(new ChooseToolPage(configuration, ls));
	} else {
		frame->switchPage(new IntroPage(configuration));
	}

	frame->Show(true);

	return true;
}

void ScummVMToolsApp::OnAbout() {
	wxDialog *dialog = new wxDialog(NULL, wxID_ANY, wxT("About"), wxDefaultPosition, wxSize(500, 380));

	wxSizer *topsizer = new wxBoxSizer(wxVERTICAL);

	topsizer->Add(new Header(dialog, wxT("detaillogo.jpg"), wxT("tile.gif"), wxT("")), wxSizerFlags().Expand());

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	// Create text content

	wxStaticText *titletext = new wxStaticText(dialog, wxID_ANY, wxT("ScummTools GUI"));
	titletext->SetFont(wxFont(22, wxSWISS, wxNORMAL, wxBOLD, false, wxT("Arial")));
	sizer->Add(titletext, wxSizerFlags());

	wxStaticText *versiontext = new wxStaticText(dialog, wxID_ANY, wxT("Version 1.1.0 BETA"));
	versiontext->SetForegroundColour(wxColor(128, 128, 128));
	versiontext->SetFont(wxFont(10, wxSWISS, wxNORMAL, wxNORMAL, false, wxT("Arial")));
	sizer->Add(versiontext, wxSizerFlags());

	wxHyperlinkCtrl *websitetext = new wxHyperlinkCtrl(dialog, wxID_ANY, wxT("http://www.scummvm.org"), wxT("http://www.scummvm.org"));
	sizer->Add(websitetext, wxSizerFlags().Border(wxTOP, 5));

	wxStaticText *copyrighttext = new wxStaticText(dialog, wxID_ANY, wxT("Copyright ScummVM Team 2009-2010"));
	copyrighttext->SetFont(wxFont(8, wxSWISS, wxNORMAL, wxNORMAL, false, wxT("Arial")));
	sizer->Add(copyrighttext, wxSizerFlags());

	wxStaticText *descriptiontext = new wxStaticText(dialog, wxID_ANY,
		wxT("This tool allows you to extract data files from several different games \n")
		wxT("to be used by ScummVM, it can also compress audio data files into a more \n")
		wxT("compact format than the original."));
	descriptiontext->SetFont(wxFont(8, wxSWISS, wxNORMAL, wxNORMAL, false, wxT("Arial")));
	sizer->Add(descriptiontext, wxSizerFlags().Border(wxTOP, 6));

	wxStaticText *licensetext = new wxStaticText(dialog, wxID_ANY,
		wxT("Published under the GNU General Public License\n")
		wxT("This program comes with ABSOLUTELY NO WARRANTY\n")
		wxT("This is free software, and you are welcome to redistribute it ")
		wxT("under certain conditions"));
	licensetext->SetFont(wxFont(8, wxSWISS, wxNORMAL, wxNORMAL, false, wxT("Arial")));
	sizer->Add(licensetext, wxSizerFlags().Border(wxTOP, 10));


	// Apply layout
	topsizer->Add(sizer, wxSizerFlags().Expand().Border(wxALL, 10));

	// Add the standard buttons (only one)
	topsizer->Add(dialog->CreateStdDialogButtonSizer(wxOK), wxSizerFlags().Center().Border());

	// Add the standard buttons
	dialog->SetSizerAndFit(topsizer);

	// Display it
	dialog->ShowModal();

	// Destroy it
	dialog->Destroy();
}


// Event table for the top frame
BEGIN_EVENT_TABLE(ScummToolsFrame, wxFrame)
	//EVT_MENU(wxID_PREFERENCES, ScummToolsFrame::onMenuPreferences)
	EVT_BUTTON(ID_HELP, ScummToolsFrame::onMenuHelp)
	EVT_MENU(wxID_HELP, ScummToolsFrame::onMenuHelp)
	EVT_BUTTON(ID_ADVANCED, ScummToolsFrame::onMenuAdvanced)
	EVT_MENU(ID_ADVANCED, ScummToolsFrame::onMenuAdvanced)
	EVT_MENU(ID_MANUAL, ScummToolsFrame::onMenuManual)
	EVT_MENU(ID_WEBSITE, ScummToolsFrame::onMenuWebsite)
	EVT_MENU(wxID_ABOUT, ScummToolsFrame::onMenuAbout)
	EVT_BUTTON(ID_ABOUT, ScummToolsFrame::onMenuAbout)
	EVT_MENU(wxID_EXIT, ScummToolsFrame::onMenuExit)

	EVT_IDLE(ScummToolsFrame::onIdle)
	EVT_CLOSE(ScummToolsFrame::onClose)
END_EVENT_TABLE()

ScummToolsFrame::ScummToolsFrame(const wxString &title, const wxPoint &pos, const wxSize& size)
		: wxFrame((wxFrame *)NULL, -1, title, pos, size)
{
	// Load the default configuration
	_configuration.load();


	// We need a parent frame for correct background color (default frame looks 'disabled' in the background)
	wxPanel *main = new wxPanel(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Main Panel"));

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	// Add the top header, it's sweet!
	sizer->Add(
		new Header(main, wxT("logo.jpg"), wxT("tile.gif"), wxT("Extraction & Compression Wizard")),
		wxSizerFlags(0).Expand());

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
	// Save the current configuration
	// false means we don't save audio settings
	_configuration.save(false);

	for (std::vector<WizardPage *>::iterator iter = _pages.begin(); iter != _pages.end(); ++iter)
		delete *iter;
}

void ScummToolsFrame::CreateMenuBar() {
	wxMenuBar *menubar = new wxMenuBar();

#ifdef __WXMAC__
	wxApp::s_macHelpMenuTitleName = wxT("Help");
#endif

	// Name of this seems really inappropriate
	wxMenu *helpmenu = new wxMenu();
	helpmenu->Append(wxID_HELP, wxT("Help"));
	// Might be under the wrong menu...
	helpmenu->Append(ID_ADVANCED, wxT("&Default Settings"));
	helpmenu->Append(ID_MANUAL, wxT("&Manual Page"));
	helpmenu->Append(ID_WEBSITE, wxT("Visit ScummVM &Website"));
	helpmenu->Append(wxID_ABOUT, wxT("&About ") + wxGetApp().GetAppName());
	menubar->Append(helpmenu, wxT("Help"));

	SetMenuBar(menubar);
}

void ScummToolsFrame::switchPage(WizardPage *next, bool moveback) {
	// Associate us with the new page
	if (next)
		next->SetScummFrame(this);

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

void ScummToolsFrame::onMenuAdvanced(wxCommandEvent &evt) {
	// We fill the temporary object with the current standard settings
	Configuration defaults;
	defaults.load();

	// Display the dialog with options
	int ok;
	{
		AdvancedSettingsDialog dlg(this, defaults);
		ok = dlg.ShowModal();
		// Settings are saved once the window is closed
	}

	// Save the settings
	if (ok == wxID_OK) {
		defaults.save();
		// Fill in values from the defaults, note that this overrides
		// current settings!
		_configuration.load();
	}
}

void ScummToolsFrame::onMenuManual(wxCommandEvent &evt) {
	// Wiki page
	::wxLaunchDefaultBrowser(wxT("http://wiki.scummvm.org/index.php/User_Manual/Appendix:_Tools"));
}

void ScummToolsFrame::onMenuWebsite(wxCommandEvent &evt) {
	::wxLaunchDefaultBrowser(wxT("http://scummvm.org"));
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

	sizer->AddSpacer(10);

	_prefs= new wxButton(this, ID_ADVANCED, wxT("Default Settings"));
	_prefs->SetSize(80, -1);
	sizer->Add(_prefs, wxSizerFlags().Left().ReserveSpaceEvenIfHidden());

#ifdef __WXMAC__
	_help->Hide();
	_prefs->Hide();
#endif

	// Insert space between the buttons
	topsizer->Add(sizer, wxSizerFlags().Left().Border());
	topsizer->Add(new wxPanel(this, wxID_ANY, wxDefaultPosition, wxDefaultSize), wxSizerFlags(1).Expand());
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

	topsizer->Add(sizer, wxSizerFlags().Right().Border());

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

// Window header

BEGIN_EVENT_TABLE(Header, wxPanel)
	EVT_PAINT(Header::onPaint)
END_EVENT_TABLE()

Header::Header(wxWindow *parent, const wxString &logo, const wxString &tile, const wxString &title)
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
#ifdef __WXMSW__
	// Windows likes subfolders for media files
	_logo.LoadFile(wxT("media/") + logo, wxBITMAP_TYPE_JPEG);
	_tile.LoadFile(wxT("media/") + tile, wxBITMAP_TYPE_GIF);
#else
	// On other platforms, files are more scattered, and we use the standard resource dir
	_logo.LoadFile(wxStandardPaths::Get().GetResourcesDir() + wxT("/") + logo, wxBITMAP_TYPE_JPEG);
	_tile.LoadFile(wxStandardPaths::Get().GetResourcesDir() + wxT("/") + tile, wxBITMAP_TYPE_GIF);
#endif

	// Load font
	_font = wxFont(10, wxFONTFAMILY_SWISS, wxFONTSTYLE_NORMAL, wxFONTWEIGHT_NORMAL, false);

	// Set the text title
	_title = title;
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
	dc.DrawText(_title, 290, 70);
}

AdvancedSettingsDialog::AdvancedSettingsDialog(wxWindow *parent, Configuration &defaults) :
	wxDialog(parent, wxID_ANY, wxT("Default Settings"), wxDefaultPosition, wxDefaultSize),
	_defaults(defaults)
{
	wxNotebook *notebook = new wxNotebook(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxNB_TOP);

	_mp3 = new ChooseAudioOptionsMp3Page(defaults);
	_flac = new ChooseAudioOptionsFlacPage(defaults);
	_vorbis = new ChooseAudioOptionsVorbisPage(defaults);

	notebook->AddPage(_mp3panel = _mp3->CreatePanel(notebook), wxT("MP3"));
	notebook->AddPage(_flacpanel = _flac->CreatePanel(notebook), wxT("Flac"));
	notebook->AddPage(_vorbispanel = _vorbis->CreatePanel(notebook), wxT("Vorbis"));

	wxSizer *topsizer = new wxBoxSizer(wxVERTICAL);

	topsizer->Add(notebook, wxSizerFlags(1).Expand().Border());
	topsizer->Add(CreateStdDialogButtonSizer(wxOK | wxCANCEL), wxSizerFlags(0).Center().Border());

	SetSizerAndFit(topsizer);
}

AdvancedSettingsDialog::~AdvancedSettingsDialog() {
	_mp3->save(_mp3panel);
	_flac->save(_flacpanel);
	_vorbis->save(_vorbispanel);
	delete _mp3;
	delete _flac;
	delete _vorbis;
}
