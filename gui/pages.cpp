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
#include "tools.h"

WizardPage::WizardPage(ScummToolsFrame *frame)
	: _topframe(frame)
{
}

wxWindow *WizardPage::CreatePanel(wxWindow *parent) {
	return new wxPanel(parent, wxID_ANY, wxDefaultPosition, wxDefaultSize, wxBORDER_NONE, wxT("Wizard Page"));
}

void WizardPage::switchPage(WizardPage *next) {
	_topframe->switchPage(next);
}

void WizardPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	// Do nothing, keep default button state
}

void WizardPage::SetAlignedSizer(wxWindow *panel, wxSizer *sizer) {
	wxSizer *topsizer = new wxBoxSizer(wxHORIZONTAL);
	topsizer->AddSpacer(100);
	topsizer->Add(sizer);
	panel->SetSizer(topsizer);
}

// Our default handler for next/prev/cancel

void WizardPage::onNext(wxWindow *panel) {
}

void WizardPage::onPrevious(wxWindow *panel) {
	_topframe->switchToPreviousPage();
}

void WizardPage::onCancel(wxWindow *panel) {
	wxMessageDialog dlg(panel, wxT("Are you sure you want to abort the wizard?"), wxT("Abort"), wxYES | wxNO);
	wxWindowID ret = dlg.ShowModal();
	if(ret == wxID_YES) {
		_topframe->Close(true);
	} else {
		// Do nothing
	}
}

// Load/Save settings
void WizardPage::save(wxWindow *panel, Configuration &configuration) {
}

// Introduction page

IntroPage::IntroPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *IntroPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Welcome to the ScummVM extraction and compression utility.")));
	sizer->Add(new wxStaticText(panel, wxID_ANY,
		wxT("Please select what you want to do, or drop a file or folder on this window for automatic .")));
	
	wxString choices[] = {
		wxT("Extract from game data files"),
		wxT("Compress audio files"),
		wxT("Choose tool to use (advanced)")
	};

	wxRadioBox *options = new wxRadioBox(panel, wxID_ANY, wxT(""), 
		wxDefaultPosition, wxDefaultSize, 3, choices, 1, 
		wxRA_SPECIFY_COLS | wxBORDER_NONE, wxDefaultValidator, wxT("ChooseActivity"));
	sizer->Add(options);
	options->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load options
	Configuration &config = _topframe->configuration;
	if(config.advanced)
		options->SetSelection(2);
	else if(config.compressing)
		options->SetSelection(1);
	else
		options->SetSelection(0);

	return panel;
}

void IntroPage::updateButtons(wxWindow *panel, WizardButtons *buttons) {
	buttons->showPrevious(false);
	buttons->enableNext(true);
}

void IntroPage::save(wxWindow *panel, Configuration &config) {
	wxString selected_option = static_cast<wxRadioBox *>(panel->FindWindowByName(wxT("")))->GetStringSelection().Lower();

	config.advanced    = selected_option.Find(wxT("advanced")) != wxNOT_FOUND;
	config.compressing = selected_option.Find(wxT("extract")) == wxNOT_FOUND;
}

void IntroPage::onNext(wxWindow *panel) {
	wxString selected_option = static_cast<wxRadioBox *>(panel->FindWindowByName(wxT("")))->GetStringSelection().Lower();
	if(selected_option.Find(wxT("extract")) != wxNOT_FOUND) {
		// extract
	} else if(selected_option.Find(wxT("advanced")) != wxNOT_FOUND) {
		// advanced
		_topframe->switchPage(new ChooseToolPage(_topframe));
	} else {
		// compress
		_topframe->switchPage(new ChooseCompressionPage(_topframe));
	}
}

// Page to choose what game files to compress

ChooseCompressionPage::ChooseCompressionPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseCompressionPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Please select for what game/engine you'd like to compress files.")));
	
	// This list is most likely incomplete
	wxArrayString choices;

	// Many games use the same tool internally, and are grouped by tool used here
	// the array is ordered before being displayed, though

	// TODO: This should be moved to tools.cpp and stored in each tool instead

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
	choices.Add(wxT("I Have No Mouth and I Must Scream")),

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

	wxChoice *game = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("GameSelection"));
	sizer->Add(game);
	game->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load already set values
	game->SetStringSelection(_topframe->configuration.selectedGame);


	return panel;
}

// Load/Save settings

void ChooseCompressionPage::save(wxWindow *panel, Configuration &config) {
	wxString game = static_cast<wxChoice *>(panel->FindWindowByName(wxT("GameSelection")))->GetStringSelection();
	config.selectedGame = game;
}

// Page to choose ANY tool to use

ChooseToolPage::ChooseToolPage(ScummToolsFrame* frame)
	: WizardPage(frame)
{
}

wxWindow *ChooseToolPage::CreatePanel(wxWindow *parent) {
	wxWindow *panel = WizardPage::CreatePanel(parent);

	wxSizer *sizer = new wxBoxSizer(wxVERTICAL);

	sizer->AddSpacer(15);

	sizer->Add(new wxStaticText(panel, wxID_ANY, 
		wxT("Select what tool you'd like to use.")));
	
	// This list is most likely incomplete
	wxArrayString choices = g_tools.getToolList();

	// Sort the array for display (it should actually always be sorted since 
	// they're stored in a ordered tree but you can never be too safe)
	choices.Sort();

	wxChoice *tool = new wxChoice(panel, wxID_ANY, wxDefaultPosition, wxDefaultSize, 
		choices, 0, wxDefaultValidator, wxT("ToolSelection"));
	sizer->Add(tool);
	tool->SetSelection(0);

	SetAlignedSizer(panel, sizer);

	// Load configuration
	tool->SetStringSelection(_topframe->configuration.selectedTool);

	return panel;
}

// Load/Save settings
void ChooseToolPage::save(wxWindow *panel, Configuration &config) {
	config.selectedTool = static_cast<wxChoice *>(panel->FindWindowByName(wxT("ToolSelection")))->GetStringSelection();
}
