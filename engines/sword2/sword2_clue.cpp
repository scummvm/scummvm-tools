/* clue - Extremely primitive "CLUster Explorer" for Broken Sword 2
 * Copyright (C) 2006 The ScummVM project
 *
 * This program is free software; you can redistribute it and/or
 * modify it under the terms of the GNU General Public License
 * as published by the Free Software Foundation; either version 2
 * of the License, or (at your option) any later version.
 *
 * This program is distributed in the hope that it will be useful,
 * but WITHOUT ANY WARRANTY; without even the implied warranty of
 * MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
 * GNU General Public License for more details.
 *
 * You should have received a copy of the GNU General Public License
 * along with this program; if not, write to the Free Software
 * Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA 02110-1301, USA.
 */

#include "common/file.h"
#include "common/util.h"

#include <gtk/gtk.h>

// The following cluster files are - to some extent - understood:
//
// carib1.clu
//    247 ANIMATION_FILE resources
//      7 SCREEN_FILE resources
//     11 WALK_GRID_FILE resources
//      7 RUN_LIST resources
//     24 WAV_FILE resources
// carib2.clu
//    133 ANIMATION_FILE resources
//     11 SCREEN_FILE resources
//     13 WALK_GRID_FILE resources
//     11 RUN_LIST resources
//     30 WAV_FILE resources
// carib3.clu
//    174 ANIMATION_FILE resources
//      5 SCREEN_FILE resources
//      4 WALK_GRID_FILE resources
//      5 RUN_LIST resources
//     31 WAV_FILE resources
// docks.clu
//    216 ANIMATION_FILE resources
//      4 SCREEN_FILE resources
//      3 WALK_GRID_FILE resources
//      4 RUN_LIST resources
//     47 WAV_FILE resources
//      1 PALETTE_FILE resource
// general.clu
//     62 ANIMATION_FILE resources
//      7 SCREEN_FILE resources
//      1 WALK_GRID_FILE resource
//     89 MOUSE_FILE resources
//      1 WAV_FILE resource
//    159 ICON_FILE resources
//      1 PALETTE_FILE resource
// jungle.clu
//    137 ANIMATION_FILE resources
//      4 SCREEN_FILE resources
//      6 WALK_GRID_FILE resources
//      4 RUN_LIST resources
//     10 WAV_FILE resources
// paris.clu
//    223 ANIMATION_FILE resources
//      9 SCREEN_FILE resources
//      8 WALK_GRID_FILE resources
//      9 RUN_LIST resources
//     44 WAV_FILE resources
// players.clu
//    143 ANIMATION_FILE resources
//      2 RUN_LIST resources
// pyramid1.clu
//    189 ANIMATION_FILE resources
//      4 SCREEN_FILE resources
//     10 WALK_GRID_FILE resources
//      4 RUN_LIST resources
//     14 WAV_FILE resources
//     30 PALETTE_FILE resources
// pyramid2.clu
//    124 ANIMATION_FILE resources
//      6 SCREEN_FILE resources
//      5 WALK_GRID_FILE resources
//      6 RUN_LIST resources
//     35 WAV_FILE resources
//    130 PALETTE_FILE resources
// quaramon.clu
//    288 ANIMATION_FILE resources
//      5 SCREEN_FILE resources
//     16 WALK_GRID_FILE resources
//      5 RUN_LIST resources
//     23 WAV_FILE resources
// scripts.clu
//    905 GAME_OBJECT resources
//      1 GLOBAL_VAR_FILE resource
//     68 SCREEN_MANAGER resources
// text.clu
//    112 TEXT_FILE resources
// warehous.clu
//    174 ANIMATION_FILE resources
//      5 SCREEN_FILE resources
//      7 WALK_GRID_FILE resources
//      5 RUN_LIST resources
//     41 WAV_FILE resources
//
// The following files are NOT understood:
//
// credits.clu     - This is a text file, with some formatting information
// font.clu        - Credits font?
// music1.clu      - See compress_sword2.c
// music2.clu      - See compress_sword2.c
// speech1.clu     - See compress_sword2.c
// speech2.clu     - See compress_sword2.c

enum {
	NAME_COLUMN,
	SIZE_COLUMN,
	TYPE_COLUMN,
	POSITION_COLUMN,
	LENGTH_COLUMN,
	N_COLUMNS
};

enum {
	ANIMATION_FILE		= 1,
	SCREEN_FILE		= 2,
	GAME_OBJECT		= 3,
	WALK_GRID_FILE		= 4,
	GLOBAL_VAR_FILE		= 5,
	PARALLAX_FILE_null	= 6,
	RUN_LIST		= 7,
	TEXT_FILE		= 8,
	SCREEN_MANAGER		= 9,
	MOUSE_FILE		= 10,
	WAV_FILE		= 11,
	ICON_FILE		= 12,
	PALETTE_FILE		= 13
};

void open_screen_file(char *filename, uint32 pos, uint32 len) {
	g_message("Would open a screen file here");
}

const gchar *getType(byte type) {
	switch (type) {
	case ANIMATION_FILE:
		return "Animation";
	case SCREEN_FILE:
		return "Screen";
	case GAME_OBJECT:
		return "Object";
	case WALK_GRID_FILE:
		return "Walk Grid";
	case GLOBAL_VAR_FILE:
		return "Global Variables";
	case PARALLAX_FILE_null:
		return "Parallax File (null";
	case RUN_LIST:
		return "Run List";
	case TEXT_FILE:
		return "Text File";
	case SCREEN_MANAGER:
		return "Screen Manager";
	case MOUSE_FILE:
		return "Mouse File";
	case WAV_FILE:
		return "WAV File";
	case ICON_FILE:
		return "Icon";
	case PALETTE_FILE:
		return "Palette";
	default:
		return "<unknown>";
	}
}

void main_window_destroy_cb(GtkWidget *widget, gpointer data) {
	gtk_main_quit();
}

gboolean tree_view_button_cb(GtkTreeView *view, GdkEventButton *event, gpointer data) {
	gchar *filename = (gchar *)data;
	GtkTreeSelection *selection;
	GtkTreeIter iter;
	GtkTreeModel *model;
	gchar *name;
	gint type, pos, len;

	if (event->type != GDK_2BUTTON_PRESS || event->button != 1)
		return FALSE;

	selection = gtk_tree_view_get_selection(view);
	if (!gtk_tree_selection_get_selected(selection, &model, &iter))
		return FALSE;

	gtk_tree_model_get(model, &iter,
		NAME_COLUMN, &name,
		TYPE_COLUMN, &type,
		POSITION_COLUMN, &pos,
		LENGTH_COLUMN, &len,
		-1);

	switch (type) {
	case SCREEN_FILE:
		open_screen_file(filename, pos, len);
		return TRUE;

	case ANIMATION_FILE:
	case GAME_OBJECT:
	case WALK_GRID_FILE:
	case GLOBAL_VAR_FILE:
	case PARALLAX_FILE_null:
	case RUN_LIST:
	case TEXT_FILE:
	case SCREEN_MANAGER:
	case MOUSE_FILE:
	case WAV_FILE:
	case ICON_FILE:
	case PALETTE_FILE:
		g_message("'%s' is in '%s' at position %d, length %d", name, filename, pos, len);
		return TRUE;
	default:
		if (type != -1)
			g_message("Unknown resource type: %d", type);
		return FALSE;
	}
}

gint compare_items(GtkTreeModel *model, GtkTreeIter *a, GtkTreeIter *b, gpointer user_data) {
	char *name1, *name2;

	gtk_tree_model_get(model, a, NAME_COLUMN, &name1, -1);
	gtk_tree_model_get(model, b, NAME_COLUMN, &name2, -1);

	return strcmp(name1, name2);
}

gchar *make_size(uint32 len) {
	static gchar size[10];

	if (len >= 0x100000)
		sprintf(size, "%0.1fM", (double)len / 0x100000);
	else if (len >= 0x400)
		sprintf(size, "%dK", len / 0x400);
	else
		sprintf(size, "%d", len);

	return size;
}

int main(int argc, char *argv[]) {
	GtkWidget *main_window, *scroll;
	GtkWidget *treeview;
	GtkTreeViewColumn *column;
	GtkCellRenderer *name_renderer, *size_renderer;
	GtkTreeStore *store;
	GtkTreeIter categories[14];
	GValue value = { 0, };
	gint offset;
	uint32 res_counts[14];
	uint32 res_sizes[14];
	int i;

	Common::File in;
	uint32 index_pos;
	uint32 pos, len;

	gtk_init(&argc, &argv);

	if (argc != 2) {
		printf("Usage: %s filename\n", argv[0]);
		return EXIT_FAILURE;
	}

	in.open(argv[1], "rb");
	if (!in.isOpen()) {
		printf("Couldn't open %s for reading\n", argv[1]);
		return EXIT_FAILURE;
	}

	/* Create the main window, scrollable in both directions */

	main_window = gtk_window_new(GTK_WINDOW_TOPLEVEL);
	gtk_window_set_title(GTK_WINDOW(main_window), "CLUster Explorer");
	gtk_window_set_default_size(GTK_WINDOW(main_window), 400, 400);

	g_signal_connect(G_OBJECT(main_window), "destroy", G_CALLBACK(main_window_destroy_cb), NULL);

	scroll = gtk_scrolled_window_new(NULL, NULL);
	gtk_scrolled_window_set_shadow_type(GTK_SCROLLED_WINDOW(scroll), GTK_SHADOW_ETCHED_IN);
	gtk_scrolled_window_set_policy(GTK_SCROLLED_WINDOW(scroll), GTK_POLICY_AUTOMATIC, GTK_POLICY_AUTOMATIC);

	/* Create the tree view */

	for (i = 0; i < ARRAYSIZE(res_counts); i++) {
		res_counts[i] = 0;
		res_sizes[i] = 0;
	}

	store = gtk_tree_store_new(N_COLUMNS,
		G_TYPE_STRING,
		G_TYPE_STRING,
		G_TYPE_INT,
		G_TYPE_INT,
		G_TYPE_INT);

	gtk_tree_sortable_set_default_sort_func(GTK_TREE_SORTABLE(store), compare_items, NULL, NULL);
	gtk_tree_sortable_set_sort_column_id(GTK_TREE_SORTABLE(store), GTK_TREE_SORTABLE_DEFAULT_SORT_COLUMN_ID, GTK_SORT_ASCENDING);

	index_pos = in.readUint32LE();
	in.seek(index_pos, SEEK_SET);

	for (;;) {
		GtkTreeIter iter;
		byte type;
		gchar *utf8_name;
		gchar name[34];
		gchar *size;

		try {
			pos = in.readUint32LE();
			len = in.readUint32LE();
		} catch (...) {
			break;
		}

		size = make_size(len);

		index_pos = in.pos();

		in.seek(pos, SEEK_SET);

		type = in.readByte();
		in.readByte();				/* compType	*/
		in.readUint32LE();			/* compSize	*/
		in.readUint32LE();			/* decompSize	*/
		in.read_noThrow(name, sizeof(name));

		/*
		 * We need to convert from Latin-1 to UTF-8. Otherwise the text
		 * "CAFÉ" won't be displayed properly.
		 */

		utf8_name = g_convert(name, -1, "UTF-8", "ISO-8859-1", NULL, NULL, NULL);

		if (!res_counts[type]) {
			gtk_tree_store_append(store, &categories[type], NULL);
			gtk_tree_store_set(store, &categories[type],
				NAME_COLUMN, getType(type),
				SIZE_COLUMN, "",
				TYPE_COLUMN, -1,
				POSITION_COLUMN, -1,
				LENGTH_COLUMN, -1,
				-1);
		}

		res_counts[type]++;
		res_sizes[type] += len;
		gtk_tree_store_append(store, &iter, &categories[type]);
		gtk_tree_store_set(store, &iter,
			NAME_COLUMN, utf8_name,
			SIZE_COLUMN, size,
			TYPE_COLUMN, type,
			POSITION_COLUMN, pos,
			LENGTH_COLUMN, len);

		in.seek(index_pos, SEEK_SET);
	}

	in.close();

	for (i = 0; i < ARRAYSIZE(res_counts); i++) {
		if (res_counts[i]) {
			gchar size[80];

			sprintf(size, "%s [%d]", make_size(res_sizes[i]), res_counts[i]);
			gtk_tree_store_set(store, &categories[i],
				SIZE_COLUMN, size,
				-1);
		}
	}

	treeview = gtk_tree_view_new_with_model(GTK_TREE_MODEL(store));
	gtk_tree_view_set_headers_visible(GTK_TREE_VIEW(treeview), TRUE);

	g_signal_connect(G_OBJECT(treeview), "button-press-event", G_CALLBACK(tree_view_button_cb), argv[1]);

	/* The view now holds a reference. We can get rid of our own. */
	g_object_unref(G_OBJECT(store));

	name_renderer = gtk_cell_renderer_text_new();
	size_renderer = gtk_cell_renderer_text_new();

	g_value_init(&value, G_TYPE_FLOAT);
	g_value_set_float(&value, 1.0);
	g_object_set_property(G_OBJECT(size_renderer), "xalign", &value);

	gtk_tree_view_insert_column_with_attributes(
		GTK_TREE_VIEW(treeview), -1, "Name", name_renderer,
		"text", NAME_COLUMN,
		NULL);

	offset = gtk_tree_view_insert_column_with_attributes(
		GTK_TREE_VIEW(treeview), -1, "Size", size_renderer,
		"text", SIZE_COLUMN,
		NULL);
	column = gtk_tree_view_get_column(GTK_TREE_VIEW(treeview), offset - 1);
	gtk_tree_view_column_set_alignment(column, 1.0);

	gtk_container_add(GTK_CONTAINER(scroll), GTK_WIDGET(treeview));
	gtk_container_add(GTK_CONTAINER(main_window), scroll);
	gtk_widget_show_all(GTK_WIDGET(main_window));
	gtk_main();

	return EXIT_SUCCESS;
}
