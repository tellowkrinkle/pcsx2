/*
 *	Copyright (C) 2020 Pcsx2 Team
 *
 *  This Program is free software; you can redistribute it and/or modify
 *  it under the terms of the GNU General Public License as published by
 *  the Free Software Foundation; either version 2, or (at your option)
 *  any later version.
 *
 *  This Program is distributed in the hope that it will be useful,
 *  but WITHOUT ANY WARRANTY; without even the implied warranty of
 *  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE. See the
 *  GNU General Public License for more details.
 *
 *  You should have received a copy of the GNU General Public License
 *  along with GNU Make; see the file COPYING.  If not, write to
 *  the Free Software Foundation, Inc., 51 Franklin Street, Fifth Floor, Boston, MA  02110-1301, USA USA.
 *  http://www.gnu.org/copyleft/gpl.html
 *
 */

#pragma once

#include "stdafx.h"
#include "GS.h"
#include "GSdx.h"
#include "GSdxResources.h"
#include "GSSetting.h"

#include <wx/wx.h>
#include <wx/stdpaths.h>
#include <wx/filename.h>
#include <wx/notebook.h>
#include <wx/spinctrl.h>
#include <wx/wrapsizer.h>

class RendererTab : public wxPanel
{
    wxCheckBox *hacks_check, *eight_bit_check, *framebuffer_check, *flush_check, *edge_check, *mipmap_check;
    wxChoice *m_res_select, *m_anisotropic_select, *m_mipmap_select, *m_crc_select, *m_date_select, *m_blend_select;
    wxSpinCtrl *thread_spin;

public:
    RendererTab(wxWindow *parent);
};

class AdvancedTab : public wxPanel
{

public:
    AdvancedTab(wxWindow *parent);
};

class DebugTab : public wxPanel
{

public:
    DebugTab(wxWindow *parent);
};

class PostTab : public wxPanel
{

public:
    PostTab(wxWindow *parent);
};

class OSDTab : public wxPanel
{

public:
    OSDTab(wxWindow *parent);
};

class Dialog : public wxDialog
{
    wxBoxSizer *m_top_box;
    wxArrayString m_renderer_str, m_interlace_str, m_texture_str;
    wxChoice *m_renderer_select, *m_interlace_select, *m_texture_select;
    RendererTab *m_renderer_panel;
    AdvancedTab *m_adv_panel;
    DebugTab *m_debug_rec_panel;
    PostTab *m_post_panel;
    OSDTab *m_osd_panel;

public:
    Dialog();
    ~Dialog();
    void Reset();
    void Save();
};

extern bool RunwxDialog();