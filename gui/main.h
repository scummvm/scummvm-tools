/* ScummVM Tools
 *
 * ScummVM Tools is the legal property of its developers, whose names
 * are too numerous to list here. Please refer to the COPYRIGHT
 * file distributed with this source distribution.
 *
 * This program is free software: you can redistribute it and/or modify
 * it under the terms of the GNU General Public License as published by
 * the Free Software Foundation, either version 3 of the License, or
 * (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program.  If not, see <http://www.gnu.org/licenses/>.
 *
 */

/* Main  window classes for the tool GUI */

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
	ID_HELP,
	ID_ABOUT,
	ID_COMPRESS,
	ID_EXTRACT,
	ID_ADVANCED,
	ID_WEBSITE,
	ID_MANUAL
};

/**
 * Top window of the application, the entire wizard, simply put
 * Responsible for both the UI, and the management of pages
 */
class ScummToolsFrame : public wxFrame
{
public:
	/**
	 * Creates the top window
	 *
	 * @param title The title of the window
	 * @param pos Position of the window onscreen
	 * @param size The target size of the window
	 */
	ScummToolsFrame(const wxString &title, const wxPoint &pos, const wxSize &size);
	~ScummToolsFrame();

	/**
	 * Creates the menu bar for the application
	 * only called when running under mac osx (as menu bars are ugly in wizards when they are part of the window)
	 */
	void CreateMenuBar();

	/**
	 * Switches page, old page is stored on a stack for going backwards
	 * Buttons are reset, and state is saved
	 *
	 * @param nextPage The page to switch too, the old page window will be destroyed
	 *                 and this page displayed in it's stead.
	 */
	void switchPage(WizardPage *nextPage) {
		switchPage(nextPage, NextPage);}

	/**
	 * Switches back page to the previous page
	 * The current page is destroyed.
	 */
	void switchToPreviousPage() {
		switchPage(NULL, PreviousPage);}

	/**
	 * Switches to the first page, the stack of old pages is emptied
	 * and the current pag is destroyed.
	 */
	void switchToFirstPage() {
		switchPage(NULL, FirstPage);}

	/**
	 * Get help text of this window
	 */
	virtual wxString GetHelpText();

	/**
	 * Handle wx OnIdle events
	 */
	void onIdle(wxIdleEvent &evt);

	/**
	 * Handle application close event
	 */
	void onClose(wxCloseEvent &evt);

	void onMenuAdvanced(wxCommandEvent &evt);
	void onMenuHelp(wxCommandEvent &evt);
	void onMenuWebsite(wxCommandEvent &evt);
	void onMenuManual(wxCommandEvent &evt);
	void onMenuAbout(wxCommandEvent &evt);
	void onMenuExit(wxCommandEvent &evt);

	/** The state of the wizard so far */
	Configuration _configuration;

	/** The button pane */
	WizardButtons *_buttons;
private:
	enum SwitchToPage { NextPage, PreviousPage, FirstPage };
	void switchPage(WizardPage *nextPage, SwitchToPage page);
    
	void destroyOldPanels();

	wxPanel *_wizardpane;

	std::vector<WizardPage *> _pages;
	std::vector<wxWindow *> _oldPanels;

	DECLARE_EVENT_TABLE()
};

/**
 * This panel contains the "Next", "Previous" and "Abort" buttons
 * Convenient interface as you don't need to create buttons
 * with long, awkward calls
 */

class WizardButtons : public wxPanel {
public:
	/**
	 * Creates the button window.
	 *
	 * @param parent The parent window
	 * @param linetext A static text that is used to display some extra information
	 * @param conf The configuration object used for the tools
	 */
	WizardButtons(wxWindow *parent, wxStaticText *linetext, Configuration &conf);

	/* Set the buttons to the standard configuration
	 * (prev, next shown and enabled, finish disabled)
	 */
	void reset();

	/**
	 * Set the current wizard page, this is called from SwitchPage to make us
	 * know where to drop events generated by the buttons
	 */
	void setPage(WizardPage *current, wxWindow *panel);

	/**
	 * Set the label of the line above the buttons, can display some useful info here
	 *
	 * @param label The label to display
	 */
	void setLineLabel(wxString label);

	/**
	 * Enables the next button (or finish for the last page)
	 */
	void enableNext(bool enable);

	/**
	 * Enables the previous button, shows it if it's disabled
	 */
	void enablePrevious(bool enable);

	/**
	 * Display the previous/next button.
	 */
	void showNavigation(bool show);

	/**
	 * Display the previous button.
	 * If the showNavigation has been used, this overrides that (for the previous button).
	 */
	void showPrevious(bool show);

	/**
	 * Changes name of the 'next' button to finish
	 */
	void showFinish(bool show);

	/**
	 * Changes name of the 'Cancel' button to abort
	 */
	void showAbort(bool show);

	// wx event handlers
	void onClickHelp(wxCommandEvent &e);
	void onClickAbout(wxCommandEvent &e);
	void onClickNext(wxCommandEvent &e);
	void onClickPrevious(wxCommandEvent &e);
	void onClickCancel(wxCommandEvent &e);

protected:
	/** Configuration used by the Wizard. */
	Configuration &_configuration;
	/** 'Next' (or finish) button. */
	wxButton *_next;
	/** 'Previous' button. */
	wxButton *_prev;
	/** 'Help' button. */
	wxButton *_help;
	/** 'Preferences' button */
	wxButton *_prefs;
	/** 'Cancel' button. */
	wxButton *_cancel;
	/** The static text on the line seperating the page area and the buttons. */
	wxStaticText *_linetext;
	/** Current page, required for dumping events to it. */
	WizardPage *_currentPage;
	/** Current panel, required so we can pass it as arguments to the handlers. */
	wxWindow *_currentPanel;

	DECLARE_EVENT_TABLE()
};

/**
 * The header at the top of the window
 * Tries to load media/logo.gif and media/tiled.gif
 * If unavailable, it will simply be orange/brigt orange
 */

class Header : public wxPanel
{
public:
	Header(wxWindow *parent, const wxString &logo, const wxString &tile, const wxString &title = wxT(""));

	void onPaint(wxPaintEvent &evt);
protected:
	wxBitmap loadBitmapFile(const wxString&, wxBitmapType);

	wxFont _font;
	wxBitmap _logo;
	wxBitmap _tile;
	wxString _title;

	DECLARE_EVENT_TABLE()
};

/**
 *
 */

class ChooseAudioOptionsMp3Page;
class ChooseAudioOptionsFlacPage;
class ChooseAudioOptionsVorbisPage;

class AdvancedSettingsDialog : public wxDialog
{
public:
	AdvancedSettingsDialog(wxWindow *parent, Configuration &defaults);
	~AdvancedSettingsDialog();

protected:
	Configuration &_defaults;

	ChooseAudioOptionsMp3Page *_mp3;
	wxWindow *_mp3panel;
	ChooseAudioOptionsFlacPage *_flac;
	wxWindow *_flacpanel;
	ChooseAudioOptionsVorbisPage *_vorbis;
	wxWindow *_vorbispanel;
};

#endif
