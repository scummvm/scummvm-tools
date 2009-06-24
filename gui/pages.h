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

#include "configuration.h"

// Wizard Page
// A page in the extraction wizard

class WizardPage : public wxEvtHandler
{
public:
	WizardPage(ScummToolsFrame* frame);
	~WizardPage() {
;
	}

	// Creates a visual representation of this page as a child problem of the supplied parent
	virtual wxWindow *CreatePanel(wxWindow *parent);

	void switchPage(WizardPage *next);

	// Load/Save configuration, reads data from the panel supplied
	virtual void save(wxWindow *panel);
	
	// Event handlers
	// overload these to handle prev/next/cancel clicks
	virtual void onNext(wxWindow *panel);
	virtual void onPrevious(wxWindow *panel);
	virtual void onCancel(wxWindow *panel); // Default is to display 'Are you sure' and quit if you click 'Yes'

	// Update button states
	virtual void updateButtons(wxWindow *panel, WizardButtons *buttons);

protected:
	// This adds an offset (about 100px) to the left of the sizer
	// to center the text somewhat, before adding it to the panel
	void SetAlignedSizer(wxWindow *panel, wxSizer *sizer);

	ScummToolsFrame* _topframe;
	Configuration &_configuration;
};

// Introduction page, with options to extract/compress
// Step 1

class IntroPage : public WizardPage
{
public:
	IntroPage(ScummToolsFrame* frame);
	
	wxWindow *CreatePanel(wxWindow *parent);

	void save(wxWindow *panel);

	void onNext(wxWindow *panel);
	
	void updateButtons(wxWindow *panel, WizardButtons *buttons);
};

// Step 2, choose tool / game

class ChooseCompressionPage : public WizardPage
{
public:
	ChooseCompressionPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

class ChooseExtractionPage : public WizardPage
{
public:
	ChooseExtractionPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

class ChooseToolPage : public WizardPage
{
public:
	ChooseToolPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

// Step 3, choose input & output directory/file

class ChooseInOutPage : public WizardPage
{
public:
	ChooseInOutPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};

// Step 4, choose audio format

class ChooseAudioFormatPage : public WizardPage
{
public:
	ChooseAudioFormatPage(ScummToolsFrame* frame);

	wxWindow *CreatePanel(wxWindow *parent);

	//void onNext(wxWindow *panel);

	void save(wxWindow *panel);
};
