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

class WizardPage : public wxPanel
{
public:
	WizardPage(wxWindow *parent);
	virtual void updateButtons();

	// This adds an offset (about 100px) to the left of the sizer
	// to center the text somewhat, before adding it to the panel
	void SetAlignedSizer(wxSizer *sizer);

	// This calls parent -> SwitchPage
	// This page WILL BE DELETED
	// You should return out of this class immedietly after calling this function
	void SwitchPage(WizardPage *next);

	// Load/Save configuration
	virtual void load(Configuration &configuration);
	virtual void save(Configuration &configuration);
	
	// Event handlers
	// overload these to handle prev/next/cancel clicks
	virtual void onNext() {}
	virtual void onPrevious() {}
	virtual void onCancel(); // Default is to display 'Are you sure' and quit if you click 'Yes'

	// Calls updateButtons
	void onUpdateButtons(WizardButtons *buttons);

protected:
	WizardPage *_nextPage;
	WizardPage *_prevPage;
	WizardButtons *_buttons;
};

// Introduction page, with options to extract/compress

class IntroPage : public WizardPage
{
public:
	IntroPage(wxWindow *parent);
	virtual void updateButtons();

	void load(Configuration &configuration);
	void save(Configuration &configuration);

	virtual void onNext();

protected:
	wxRadioBox *_options;
};

class CompressionPage : public WizardPage
{
public:
	CompressionPage(wxWindow *parent);

	void load(Configuration &configuration);
	void save(Configuration &configuration);

	virtual void onPrevious();

protected:
	wxChoice *_game;
};
