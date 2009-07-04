/* pages.h - All the pages in the wizard
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
 * $URL
 * $Id
 *
 */

#include <wx/wx.h>
#include <wx/process.h>

#include "configuration.h"

/**
 * A backend of a page in the wizard 
 * This class is decoupled from the UI, and you can spawn as many pages as you like from this template
 * 
 * @todo Add the ability to not have to have a ScummToolsFrame as parent, to be able to put the pages
 *       in a default config window, for example.
 */

class WizardPage : public wxEvtHandler
{
public:
	WizardPage(ScummToolsFrame* frame);
	~WizardPage() {
;
	}

	/**
	 * Creates a visual representation of this page as a child problem of the supplied parent
	 * Values will be loaded from the configuration object stored by the ScummToolsFrame
	 * 
	 * @param parent The parent window, the page will be a direct child window of this window.
	 * @return       The created window.
	 */
	virtual wxWindow *CreatePanel(wxWindow *parent);

	/**
	 * Simply calls ScummToolsFrame::switchPage
	 *
	 * @param next The window to switch to.
	 */
	void switchPage(WizardPage *next);

	/**
	 * Saves the state of the panel passed as the argument in the configuration object of
	 * the ScummToolsFrame that was passed to the constructor
	 *
	 * @param panel Panel to read data from, should be a window created by CreatePanel
	 */ 
	virtual void save(wxWindow *panel);
	
	// Event handlers
	
	/**
	 * This handler is called when the user clicks the Next button, if you overload this
	 * you should generally just call switchPage with the next page in the wizard
	 *
	 * @param panel The panel associated with this page, should have bene created by CreatePanel previously.
	 */
	virtual void onNext(wxWindow *panel);

	/**
	 * This handler is called when the user clicks the Previous button, you do not generally
	 * need to overload this as the default handler returns to the privous page already.
	 *
	 * @param panel The panel associated with this page, should have bene created by CreatePanel previously.
	 */
	virtual void onPrevious(wxWindow *panel);

	/**
	 * This handler is called when the user clicks the Cancel button, overlaod this to destroy any
	 * state this page might hold (the destructor will still be called though, so it's doubtful there
	 * is a reason to overload this). The default handler queries the user if the really want to close
	 * the wizard.
	 *
	 * @param panel The panel associated with this page, should have bene created by CreatePanel previously.
	 */
	virtual void onCancel(wxWindow *panel); // Default is to display 'Are you sure' and quit if you click 'Yes'

	/**
	 * Update state of the next/prev/cancel buttons, by default all buttons are shown, and the next button
	 * is in "next" state (ie. finish is not displayed instead of next)
	 *
	 * @param panel The panel associated with this page, should have bene created by CreatePanel previously.
	 * @param buttons The window that holds the concerned buttons
	 */
	virtual void updateButtons(wxWindow *panel, WizardButtons *buttons);

	/**
	 * This handler is called when the application is idle, used to read output from the subprocess.
	 *
	 * @return false indicates that we do not want to receive more idle events.
	 */
	virtual bool onIdle(wxPanel *panel);

protected:
	/** 
	 * This adds an offset (about 100px) to the left of the sizer to center the text somewhat, before adding it 
	 * to the panel using wxWindow::SetSizer.
	 *
	 * @param panel The panel associated with this page, should have bene created by CreatePanel previously.
	 * @param sizer The topsizer of the window.
	 */
	void SetAlignedSizer(wxWindow *panel, wxSizer *sizer);

	ScummToolsFrame* _topframe;
	Configuration &_configuration;

	DECLARE_EVENT_TABLE()
};

/**
 * Introduction page. Introduces the option to extract, compress or the advanced route (choose tool manually)
 * 
 * @todo Add the ability to drag & drop files onto this window, to automatically detect whether to compress or extract
 */

class IntroPage : public WizardPage
{
public:
	IntroPage(ScummToolsFrame* frame);
	
	wxWindow *CreatePanel(wxWindow *parent);

	void save(wxWindow *panel);

	void onNext(wxWindow *panel);
	
	void updateButtons(wxWindow *panel, WizardButtons *buttons);
};

/**
 * Presents a list of games that are supported for compression.
 * 
 * @todo Possibly merge with ChooseExtractionPage
 */

class ChooseCompressionPage : public WizardPage
{
public:
	ChooseCompressionPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

/**
 * Presents a list of games that are supported for extraction.
 * 
 * @todo Possibly merge with ChooseCompressionPage
 */

class ChooseExtractionPage : public WizardPage
{
public:
	ChooseExtractionPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

/**
 * Presents a list of all supported tools, the "advanced route"
 */

class ChooseToolPage : public WizardPage
{
public:
	ChooseToolPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

/**
 * Presents a list of all input and outputs supported by the selected tool
 * expects the configuration to already contain the selected tool
 *
 * @todo Make it look better and save state
 */

class ChooseInOutPage : public WizardPage
{
public:
	ChooseInOutPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

/**
 * Presents a dropdown list of the three different audio compression methods
 * or possibly fewer, if the selected tool does not support all methods.
 *
 * @todo Make it look better and save state, and possibly skip it if the tool
 *       only support one method of compression.
 */

class ChooseAudioFormatPage : public WizardPage
{
public:
	ChooseAudioFormatPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

/**
 * Presents advanced audio settings for the MP3 compression format
 *
 */

class ChooseAudioOptionsMp3Page : public WizardPage
{
public:
	ChooseAudioOptionsMp3Page(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);

	/**
	 * Handles clicks on the radio buttons for VBR / ABR
	 */
	void onChangeCompressionType(wxCommandEvent &evt);

	/**
	 * Enables/Disables the different fields depending on ABR/VBR setting
	 *
	 * @param panel The panel to operate on
	 */
	void updateFields(wxWindow *panel);
};

/**
 * Presents advanced audio settings for the flac compression format
 *
 */

class ChooseAudioOptionsFlacPage : public WizardPage
{
public:
	ChooseAudioOptionsFlacPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

/**
 * Presents advanced audio settings for the OGG Vorbis compression format
 *
 */

class ChooseAudioOptionsVorbisPage : public WizardPage
{
public:
	ChooseAudioOptionsVorbisPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};


/**
 * Runs the subprocess and displays it's output to the user
 * You really ought to only run one subprocess at the time, as
 * this class keeps internal state.
 */

class ProcessPage : public WizardPage
{
	/** True if the tool exited with success */
	bool _success;
	/** True if the tool has exited */
	bool _finished;
public:
	ProcessPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	/**
	 * Write to the output window pointed to by udata
	 */
	static void writeToOutput(void *udata, const char *text);

	/**
	 * Runs the specified tool, output will be put in outwin
	 *
	 * @param outwin Text control to redirect output to
	 */
	void runTool(wxTextCtrl *outwin);

	bool onIdle(wxPanel *panel);

	void onNext(wxWindow *panel);

	void updateButtons(wxWindow *panel, WizardButtons *buttons);
};

/**
 * Just displays that we extracted the files, and offers to open the target folder
 *
 */

class FinishPage : public WizardPage
{
public:
	FinishPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void updateButtons(wxWindow *panel, WizardButtons *buttons);
};
