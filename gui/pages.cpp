/* pages.cpp - All the pages in the wizard
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

#include "wx/wxprec.h"

#ifdef __BORLANDC__
	#pragma hdrstop
#endif

#ifndef WX_PRECOMP
	#include "wx/wx.h"
#endif

#include "main.h"
#include "pages.h"

WizardPage::WizardPage(wxWindow *parent)
	: wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Page")),
	  _nextPage(NULL),
	  _prevPage(NULL),
	  _buttons(NULL)
{
}

void WizardPage::updateButtons() {
	// Do nothing
}

void WizardPage::SetAlignedSizer(wxSizer *sizer) {
	wxSizer *topsizer = new wxBoxSizer(wxHORIZONTAL);
	topsizer->AddSpacer(100);
	topsizer->Add(sizer);
	SetSizer(topsizer);
}

void WizardPage::SwitchPage(WizardPage *next) {
	wxWindow *grandparent = GetParent();
	while(grandparent->GetParent() != NULL)
		grandparent = grandparent->GetParent();

	if(grandparent) {
		ScummToolsFrame *frame = dynamic_cast<ScummToolsFrame*>(grandparent);
		frame->SwitchPage(next);
		// We are probably dead now, make sure to do nothing 
		// involving member variabls after this point
	}
}

// Our default handler for cancel
void WizardPage::onCancel() {
	wxMessageDialog dlg(this, wxT("Are you sure you want to abort the wizard?"), wxT("Abort"), wxYES | wxNO);
	wxWindowID ret = dlg.ShowModal();
	if(ret == wxID_YES) {
		wxWindow *grandparent = GetParent();
		while(grandparent->GetParent() != NULL)
			grandparent = grandparent->GetParent();

		grandparent->Close(true);
	} else {
		// Do nothing
	}
}

void WizardPage::onUpdateButtons(WizardButtons *buttons) {
	// We have this functions to avoid having to this in every child handler
	_buttons = buttons;
	updateButtons();
}

// Load/Save settings
void WizardPage::load(Configuration &configuration) {
}

void WizardPage::save(Configuration &configuration) {
}

// Introduction page

IntroPage::IntroPage(wxWindow *parent)
	: WizardPage(parent)
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
}

void IntroPage::updateButtons() {
	_buttons->showPrevious(false);
	_buttons->enableNext(true);
}

void IntroPage::load(Configuration &config) {
	if(config.compressing)
		_options->SetSelection(1);
}

void IntroPage::save(Configuration &config) {
	config.compressing = _options->GetStringSelection().Lower().Find(wxT("extract")) == wxNOT_FOUND;
}

void IntroPage::onNext() {
	if(_options->GetStringSelection().Lower().Find(wxT("extract")) != wxNOT_FOUND) {
		// extract
	} else {
		// compress
		SwitchPage(new CompressionPage(this->GetParent()));
	}
}

// Page to choose compression tool

CompressionPage::CompressionPage(wxWindow *parent)
	: WizardPage(parent)
{
	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(this, wxID_ANY, 
		wxT("Please select for what game/engine you'd like to compress files.")));
	
	// This list is most likely incomplete
	wxArrayString choices;

	// Many games use the same tool internally, and are grouped by tool used here
	// the array is ordered before being displayed, though

	// compress_agos
	choices.Add(wxT("Feeble Files")),
	choices.Add(wxT("Simon the Sorcerer I/II")),

	// compress_gob
	choices.Add(wxT("Gobliiins (all versions)")),

	// compress_kyra
	choices.Add(wxT("The Legend of Kyrandia")),
	choices.Add(wxT("The Legend of Kyrandia: Hand of Fate")),
	choices.Add(wxT("The Legend of Kyrandia: Malcolm's Revenge")),
	choices.Add(wxT("Lands of Lore: The Throne of Chaos")),

	// compress_queen
	choices.Add(wxT("Flight of the Amazon Queen")),

	// compress_saga
	choices.Add(wxT("SAGA: Inherit The Earth")),
	choices.Add(wxT("I Have No Mouth")),
	choices.Add(wxT("I Must Scream")),

	// compress_scumm_bun
	choices.Add(wxT("The Secret of Monkey Island")),
	choices.Add(wxT("Monkey Island 2: LeChuck's Revenge")),
	choices.Add(wxT("The Curse of Monkey Island")),

	// compress_scumm_san
	// compress_scumm_sou
	// Unsure of exact games...

	// compress_sword1
	choices.Add(wxT("Broken Sword 1")),

	// compress_sword2
	choices.Add(wxT("Broken Sword 2")),

	// compress_touche
	choices.Add(wxT("Touche: The Adventures of the Fifth Musketeer")),

	// compress_tucker
	choices.Add(wxT("Bud Tucker in Double Trouble")),


	// Sort the array for display
	choices.Sort();

	_game = new wxChoice(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, choices);
	sizer->Add(_game);
	_game->SetSelection(0);

	SetAlignedSizer(sizer);
}

void CompressionPage::onPrevious() {
	// It's kinda ugly that we must know the type here, would be better
	// if we could infer previous page automatically, somehow, possibly
	// templates + inheritence could solve this?
	SwitchPage(new IntroPage(this->GetParent()));
}


// Load/Save settings
void CompressionPage::load(Configuration &config) {
	_game->SetStringSelection(config.selectedGame);
}

void CompressionPage::save(Configuration &config) {
	config.selectedGame = _game->GetStringSelection();
}
