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

#include "GSwxDialog.h"

namespace
{
	void add_tooltip(wxWindow* widget, int tooltip)
	{
		if (tooltip != -1)
			widget->SetToolTip(dialog_message(tooltip));
	}

	void add_settings_to_array_string(const std::vector<GSSetting>& s, wxArrayString& arr)
	{
		for (const GSSetting& setting : s)
		{
			if (!setting.note.empty())
				arr.Add(setting.name + " (" + setting.note + ")");
			else
				arr.Add(setting.name);
		}
	}

	size_t get_config_index(const std::vector<GSSetting>& s, const char* str)
	{
		int value = theApp.GetConfigI(str);

		for (size_t i = 0; i < s.size(); i++)
		{
			if (s[i].value == value)
				return i;
		}
		return 0;
	}

	void set_config_from_choice(const wxChoice* choice, const std::vector<GSSetting>& s, const char* str)
	{
		int idx = choice->GetSelection();

		if (idx == wxNOT_FOUND)
			return;

		theApp.SetConfig(str, s[idx].value);
	}

	void add_label(wxWindow* parent, wxSizer* sizer, const char* str, int tooltip = -1, long style = wxALIGN_RIGHT | wxALIGN_CENTRE_HORIZONTAL, wxSizerFlags flags = wxSizerFlags().Centre().Right())
	{
		auto* temp_text = new wxStaticText(parent, wxID_ANY, str, wxDefaultPosition, wxDefaultSize, style);
		add_tooltip(temp_text, tooltip);
		sizer->Add(temp_text, flags);
	}
} // namespace

GSUIElementHolder::GSUIElementHolder(wxWindow* window)
	: m_window(window)
{
}

void GSUIElementHolder::addWithLabel(wxControl* control, UIElem::Type type, wxSizer* sizer, const char* label, const char* config_name, int tooltip, wxCheckBox* prereq, wxSizerFlags flags)
{
	add_tooltip(control, tooltip);
	wxStaticText* text = new wxStaticText(m_window, wxID_ANY, label, wxDefaultPosition, wxDefaultSize, wxALIGN_RIGHT);
	add_tooltip(text, tooltip);
	sizer->Add(text, wxSizerFlags().Centre().Right());
	sizer->Add(control, flags);
	m_elems.emplace_back(type, control, config_name, prereq);
}

wxCheckBox* GSUIElementHolder::addCheckBox(wxSizer* sizer, const char* label, const char* config_name, int tooltip, wxCheckBox* prereq)
{
	wxCheckBox* box = new wxCheckBox(m_window, wxID_ANY, label);
	add_tooltip(box, tooltip);
	if (sizer)
		sizer->Add(box);
	m_elems.emplace_back(UIElem::Type::CheckBox, box, config_name, prereq);
	return box;
}

wxChoice* GSUIElementHolder::addComboBoxAndLabel(wxSizer* sizer, const char* label, const char* config_name, const std::vector<GSSetting>* settings, int tooltip, wxCheckBox* prereq)
{
	wxArrayString temp;
	add_settings_to_array_string(*settings, temp);
	wxChoice* choice = new GSwxChoice(m_window, wxID_ANY, wxDefaultPosition, wxDefaultSize, temp, settings);
	addWithLabel(choice, UIElem::Type::Choice, sizer, label, config_name, tooltip, prereq);
	return choice;
}

wxSpinCtrl* GSUIElementHolder::addSpin(wxSizer* sizer, const char* config_name, int min, int max, int initial, int tooltip, wxCheckBox* prereq)
{
	wxSpinCtrl* spin = new wxSpinCtrl(m_window, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, min, max, initial);
	add_tooltip(spin, tooltip);
	if (sizer)
		sizer->Add(spin, wxSizerFlags());
	m_elems.emplace_back(UIElem::Type::Spin, spin, config_name, prereq);
	return spin;
}

wxSpinCtrl* GSUIElementHolder::addSpinAndLabel(wxSizer* sizer, const char* label, const char* config_name, int min, int max, int initial, int tooltip, wxCheckBox* prereq)
{
	wxSpinCtrl* spin = new wxSpinCtrl(m_window, wxID_ANY, wxEmptyString, wxDefaultPosition, wxDefaultSize, wxSP_ARROW_KEYS, min, max, initial);
	addWithLabel(spin, UIElem::Type::Spin, sizer, label, config_name, tooltip, prereq, wxSizerFlags().Centre().Left());
	return spin;
}

wxSlider* GSUIElementHolder::addSliderAndLabel(wxSizer* sizer, const char* label, const char* config_name, int min, int max, int initial, int tooltip, wxCheckBox* prereq)
{
	wxSlider* slider = new wxSlider(m_window, wxID_ANY, initial, min, max, wxDefaultPosition, wxDefaultSize, wxSL_HORIZONTAL | wxSL_VALUE_LABEL);
	addWithLabel(slider, UIElem::Type::Slider, sizer, label, config_name, tooltip, prereq);
	return slider;
}

wxFilePickerCtrl* GSUIElementHolder::addFilePickerAndLabel(wxSizer* sizer, const char* label, const char* config_name, int tooltip, wxCheckBox* prereq)
{
	wxFilePickerCtrl* picker = new wxFilePickerCtrl(m_window, wxID_ANY);
	addWithLabel(picker, UIElem::Type::File, sizer, label, config_name, tooltip, prereq);
	return picker;
}

wxDirPickerCtrl* GSUIElementHolder::addDirPickerAndLabel(wxSizer* sizer, const char* label, const char* config_name, int tooltip, wxCheckBox* prereq)
{
	wxDirPickerCtrl* picker = new wxDirPickerCtrl(m_window, wxID_ANY);
	addWithLabel(picker, UIElem::Type::Directory, sizer, label, config_name, tooltip, prereq);
	return picker;
}

void GSUIElementHolder::Load()
{
	for (const UIElem& elem : m_elems)
	{
		switch (elem.type)
		{
			case UIElem::Type::CheckBox:
				static_cast<wxCheckBox*>(elem.control)->SetValue(theApp.GetConfigB(elem.config));
				break;
			case UIElem::Type::Choice:
			{
				GSwxChoice* choice = static_cast<GSwxChoice*>(elem.control);
				choice->SetSelection(get_config_index(choice->settings, elem.config));
				break;
			}
			case UIElem::Type::Spin:
				static_cast<wxSpinCtrl*>(elem.control)->SetValue(theApp.GetConfigI(elem.config));
				break;
			case UIElem::Type::Slider:
				static_cast<wxSlider*>(elem.control)->SetValue(theApp.GetConfigI(elem.config));
				break;
			case UIElem::Type::File:
			case UIElem::Type::Directory:
			{
				auto* picker = static_cast<wxFileDirPickerCtrlBase*>(elem.control);
				picker->SetInitialDirectory(theApp.GetConfigS(elem.config));
				picker->SetPath(theApp.GetConfigS(elem.config));
				break;
			}
		}
	}
}

void GSUIElementHolder::Save()
{
	for (const UIElem& elem : m_elems)
	{
		switch (elem.type)
		{
			case UIElem::Type::CheckBox:
				theApp.SetConfig(elem.config, static_cast<wxCheckBox*>(elem.control)->GetValue());
				break;
			case UIElem::Type::Choice:
			{
				GSwxChoice* choice = static_cast<GSwxChoice*>(elem.control);
				set_config_from_choice(choice, choice->settings, elem.config);
				break;
			}
			case UIElem::Type::Spin:
				theApp.SetConfig(elem.config, static_cast<wxSpinCtrl*>(elem.control)->GetValue());
				break;
			case UIElem::Type::Slider:
				theApp.SetConfig(elem.config, static_cast<wxSlider*>(elem.control)->GetValue());
				break;
			case UIElem::Type::File:
			case UIElem::Type::Directory:
				theApp.SetConfig(elem.config, static_cast<wxFileDirPickerCtrlBase*>(elem.control)->GetPath());
				break;
		}
	}
}

void GSUIElementHolder::Update()
{
	for (const UIElem& elem : m_elems)
	{
		if (elem.prereq)
			elem.control->Enable(elem.prereq->GetValue());
	}
}

RendererTab::RendererTab(wxWindow* parent)
	: wxPanel(parent, wxID_ANY)
	, m_ui(this)
{
	auto* tab_box = new wxBoxSizer(wxVERTICAL);
	auto* hardware_box = new wxStaticBoxSizer(wxVERTICAL, this, "Hardware Mode");
	auto* software_box = new wxStaticBoxSizer(wxVERTICAL, this, "Software Mode");

	auto* hw_checks_box = new wxWrapSizer(wxHORIZONTAL);

	m_ui.addCheckBox(hw_checks_box, "GPU Palette Conversion",         "paltex",                   IDC_PALTEX);
	m_ui.addCheckBox(hw_checks_box, "Conservative Buffer Allocation", "conservative_framebuffer", IDC_CONSERVATIVE_FB);
	m_ui.addCheckBox(hw_checks_box, "Accurate DATE",                  "accurate_date",            IDC_ACCURATE_DATE);

	auto* hw_choice_grid = new wxFlexGridSizer(2, 5, 5);

	m_ui.addComboBoxAndLabel(hw_choice_grid, "Internal Resolution:",   "upscale_multiplier",     &theApp.m_gs_upscale_multiplier);
	m_ui.addComboBoxAndLabel(hw_choice_grid, "Anisotropic Filtering:", "MaxAnisotropy",          &theApp.m_gs_max_anisotropy,  IDC_AFCOMBO);
	m_ui.addComboBoxAndLabel(hw_choice_grid, "Mipmapping (Insert):",   "mipmap_hw",              &theApp.m_gs_hw_mipmapping,   IDC_MIPMAP_HW);
	m_ui.addComboBoxAndLabel(hw_choice_grid, "CRC Hack Level:",        "crc_hack_level",         &theApp.m_gs_crc_level,       IDC_CRC_LEVEL);
	m_ui.addComboBoxAndLabel(hw_choice_grid, "Blending Accuracy:",     "accurate_blending_unit", &theApp.m_gs_acc_blend_level, IDC_ACCURATE_BLEND_UNIT);

	hardware_box->Add(hw_checks_box, wxSizerFlags().Centre());
	hardware_box->AddSpacer(5);
	hardware_box->Add(hw_choice_grid, wxSizerFlags().Centre());

	auto* sw_checks_box = new wxWrapSizer(wxHORIZONTAL);
	m_ui.addCheckBox(sw_checks_box, "Auto Flush",              "autoflush_sw", IDC_AUTO_FLUSH_SW);
	m_ui.addCheckBox(sw_checks_box, "Edge Antialiasing (Del)", "aa1",          IDC_AA1);
	m_ui.addCheckBox(sw_checks_box, "Mipmapping",              "mipmap",       IDC_MIPMAP_SW);

	software_box->Add(sw_checks_box, wxSizerFlags().Centre());
	software_box->AddSpacer(5);

	// Rendering threads
	auto* thread_box = new wxBoxSizer(wxHORIZONTAL);
	m_ui.addSpinAndLabel(thread_box, "Extra Rendering threads:", "extrathreads", 0, 32, 2, IDC_SWTHREADS);
	software_box->Add(thread_box, wxSizerFlags().Centre());

	tab_box->Add(hardware_box, wxSizerFlags().Centre().Expand());
	tab_box->Add(software_box, wxSizerFlags().Centre().Expand());

	SetSizerAndFit(tab_box);
	Bind(wxEVT_CHECKBOX, &RendererTab::CallUpdate, this);
}

void RendererTab::CallUpdate(wxCommandEvent& /*event*/)
{
	m_ui.Update();
}

HacksTab::HacksTab(wxWindow* parent)
	: wxPanel(parent, wxID_ANY)
	, m_ui(this)
{
	auto* tab_box = new wxBoxSizer(wxVERTICAL);

	auto* hacks_check = m_ui.addCheckBox(tab_box, "Enable User Hacks", "UserHacks");

	auto* rend_hacks_box    = new wxStaticBoxSizer(wxVERTICAL, this, "Renderer Hacks");
	auto* upscale_hacks_box = new wxStaticBoxSizer(wxVERTICAL, this, "Upscale Hacks");

	auto* rend_hacks_grid    = new wxFlexGridSizer(2, 0, 0);
	auto* upscale_hacks_grid = new wxFlexGridSizer(3, 0, 0);

	// Renderer Hacks
	m_ui.addCheckBox(rend_hacks_grid, "Auto Flush",                "UserHacks_AutoFlush",                  IDC_AUTO_FLUSH_HW,     hacks_check);
	m_ui.addCheckBox(rend_hacks_grid, "Fast Texture Invalidation", "UserHacks_DisablePartialInvalidation", IDC_FAST_TC_INV,       hacks_check);
	m_ui.addCheckBox(rend_hacks_grid, "Disable Depth Emulation",   "UserHacks_DisableDepthSupport",        IDC_TC_DEPTH,          hacks_check);
	m_ui.addCheckBox(rend_hacks_grid, "Frame Buffer Conversion",   "UserHacks_CPU_FB_Conversion",          IDC_CPU_FB_CONVERSION, hacks_check);
	m_ui.addCheckBox(rend_hacks_grid, "Disable Safe Features",     "UserHacks_Disable_Safe_Features",      IDC_SAFE_FEATURES,     hacks_check);
	m_ui.addCheckBox(rend_hacks_grid, "Memory Wrapping",           "wrap_gs_mem",                          IDC_MEMORY_WRAPPING,   hacks_check);
	m_ui.addCheckBox(rend_hacks_grid, "Preload Frame Data",        "preload_frame_with_gs_data",           IDC_PRELOAD_GS,        hacks_check);

	// Upscale
	m_ui.addCheckBox(upscale_hacks_grid, "Align Sprite",   "UserHacks_align_sprite_X",  IDC_ALIGN_SPRITE,    hacks_check);
	m_ui.addCheckBox(upscale_hacks_grid, "Merge Sprite",   "UserHacks_merge_pp_sprite", IDC_MERGE_PP_SPRITE, hacks_check);
	m_ui.addCheckBox(upscale_hacks_grid, "Wild Arms Hack", "UserHacks_WildHack",        IDC_WILDHACK,        hacks_check);

	auto* rend_hack_choice_grid    = new wxFlexGridSizer(2, 5, 5);
	auto* upscale_hack_choice_grid = new wxFlexGridSizer(2, 5, 5);

	// Renderer Hacks:
	m_ui.addComboBoxAndLabel(rend_hack_choice_grid, "Half Screen Fix:",     "UserHacks_HalfPixelOffset", &theApp.m_gs_generic_list, IDC_HALF_SCREEN_TS, hacks_check);
	m_ui.addComboBoxAndLabel(rend_hack_choice_grid, "Trilinear Filtering:", "UserHacks_TriFilter",       &theApp.m_gs_trifilter,    IDC_TRI_FILTER,     hacks_check);

	// Skipdraw Range
	add_label(this, rend_hack_choice_grid, "Skipdraw Range:", IDC_SKIPDRAWHACK);
	auto* skip_box = new wxBoxSizer(wxHORIZONTAL);
	skip_x_spin = m_ui.addSpin(skip_box, "UserHacks_SkipDraw_Offset", 0, 10000, 0, IDC_SKIPDRAWOFFSET, hacks_check);
	skip_y_spin = m_ui.addSpin(skip_box, "UserHacks_SkipDraw",        0, 10000, 0, IDC_SKIPDRAWHACK,   hacks_check);

	rend_hack_choice_grid->Add(skip_box);

	// Upscale Hacks:
	m_ui.addComboBoxAndLabel(upscale_hack_choice_grid, "Half-Pixel Offset:", "UserHacks_Half_Bottom_Override", &theApp.m_gs_offset_hack, IDC_OFFSETHACK,   hacks_check);
	m_ui.addComboBoxAndLabel(upscale_hack_choice_grid, "Round Sprite:",      "UserHacks_round_sprite_offset",  &theApp.m_gs_hack,        IDC_ROUND_SPRITE, hacks_check);

	// Texture Offsets
	add_label(this, upscale_hack_choice_grid, "Texture Offsets:", IDC_TCOFFSETX);
	auto* tex_off_box = new wxBoxSizer(wxHORIZONTAL);
	m_ui.addSpin(tex_off_box, "UserHacks_TCOffsetX", 0, 10000, 0, IDC_TCOFFSETX, hacks_check);
	m_ui.addSpin(tex_off_box, "UserHacks_TCOffsetY", 0, 10000, 0, IDC_TCOFFSETY, hacks_check);

	upscale_hack_choice_grid->Add(tex_off_box);

	rend_hacks_box->Add(rend_hacks_grid);
	rend_hacks_box->AddSpacer(5);
	rend_hacks_box->Add(rend_hack_choice_grid, wxSizerFlags().Expand());

	upscale_hacks_box->Add(upscale_hacks_grid);
	upscale_hacks_box->AddSpacer(5);
	upscale_hacks_box->Add(upscale_hack_choice_grid, wxSizerFlags().Expand());

	tab_box->Add(hacks_check, wxSizerFlags().Left());
	tab_box->Add(rend_hacks_box, wxSizerFlags().Centre().Expand());
	tab_box->Add(upscale_hacks_box, wxSizerFlags().Centre().Expand());

	SetSizerAndFit(tab_box);
	Bind(wxEVT_SPINCTRL, &HacksTab::CallUpdate, this);
	Bind(wxEVT_CHECKBOX, &HacksTab::CallUpdate, this);
}

void HacksTab::CallUpdate(wxCommandEvent& /*event*/)
{
	Update();
}

void HacksTab::Update()
{
	m_ui.Update();

	if (skip_x_spin->GetValue() == 0)
		skip_y_spin->SetValue(0);
	if (skip_y_spin->GetValue() < skip_x_spin->GetValue())
		skip_y_spin->SetValue(skip_x_spin->GetValue());
}

RecTab::RecTab(wxWindow* parent)
	: wxPanel(parent, wxID_ANY)
	, m_ui(this)
{
	auto* tab_box = new wxBoxSizer(wxVERTICAL);

	auto* record_check = m_ui.addCheckBox(tab_box, "Enable Recording (with F12)", "capture_enabled");
	auto* record_box = new wxStaticBoxSizer(wxVERTICAL, this, "Recording");
	auto* record_grid_box = new wxFlexGridSizer(2, 5, 5);
	record_grid_box->AddGrowableCol(1);

	// Resolution
	add_label(this, record_grid_box, "Resolution:");
	auto* res_box = new wxBoxSizer(wxHORIZONTAL);
	m_ui.addSpin(res_box, "CaptureWidth",  256, 8192, 640, -1, record_check);
	m_ui.addSpin(res_box, "CaptureHeight", 256, 8192, 480, -1, record_check);

	record_grid_box->Add(res_box);

	m_ui.addSpinAndLabel(record_grid_box, "Saving Threads:",        "capture_threads",       1, 32, 4, -1, record_check);
	m_ui.addSpinAndLabel(record_grid_box, "PNG Compression Level:", "png_compression_level", 1,  9, 1, -1, record_check);

	m_ui.addDirPickerAndLabel(record_grid_box, "Output Directory:", "capture_out_dir", -1, record_check);

	record_box->Add(record_grid_box, wxSizerFlags().Centre().Expand());

	tab_box->Add(record_box, wxSizerFlags().Centre().Expand());
	SetSizerAndFit(tab_box);
	Bind(wxEVT_CHECKBOX, &RecTab::CallUpdate, this);
}

void RecTab::CallUpdate(wxCommandEvent& /*event*/)
{
	m_ui.Update();
}

PostTab::PostTab(wxWindow* parent)
	: wxPanel(parent, wxID_ANY)
	, m_ui(this)
{
	auto* tab_box = new wxBoxSizer(wxVERTICAL);
	auto* shader_box = new wxStaticBoxSizer(wxVERTICAL, this, "Custom Shader");

	m_ui.addCheckBox(shader_box, "Texture Filtering of Display", "linear_present", IDC_LINEAR_PRESENT);
	m_ui.addCheckBox(shader_box, "FXAA Shader (PgUp)",           "fxaa",           IDC_FXAA);

	auto* shade_boost_check = m_ui.addCheckBox(shader_box, "Enable Shade Boost", "ShadeBoost", IDC_SHADEBOOST);

	auto* shade_boost_box = new wxStaticBoxSizer(wxVERTICAL, this, "Shade Boost");
	auto* shader_boost_grid = new wxFlexGridSizer(2, 0, 5);
	shader_boost_grid->AddGrowableCol(1);

	m_ui.addSliderAndLabel(shader_boost_grid, "Brightness:", "ShadeBoost_Brightness", 0, 100, 50, -1, shade_boost_check);
	m_ui.addSliderAndLabel(shader_boost_grid, "Contrast:",   "ShadeBoost_Contrast",   0, 100, 50, -1, shade_boost_check);
	m_ui.addSliderAndLabel(shader_boost_grid, "Saturation:", "ShadeBoost_Saturation", 0, 100, 50, -1, shade_boost_check);

	shade_boost_box->Add(shader_boost_grid, wxSizerFlags().Expand().Centre());
	shader_box->Add(shade_boost_box, wxSizerFlags().Expand().Centre());

	auto* ext_shader_check = m_ui.addCheckBox(shader_box, "Enable External Shader", "shaderfx", IDC_SHADER_FX);

	auto* ext_shader_box = new wxStaticBoxSizer(wxVERTICAL, this, "External Shader (Home)");
	auto* ext_shader_grid = new wxFlexGridSizer(2, 0, 5);
	ext_shader_grid->AddGrowableCol(1);

	m_ui.addFilePickerAndLabel(ext_shader_grid, "GLSL fx File:", "shaderfx_glsl", -1, ext_shader_check);
	m_ui.addFilePickerAndLabel(ext_shader_grid, "Config File:",  "shaderfx_conf", -1, ext_shader_check);

	ext_shader_box->Add(ext_shader_grid, wxSizerFlags().Expand().Centre());
	shader_box->Add(ext_shader_box, wxSizerFlags().Expand().Centre());

	// TV Shader
	auto* tv_box = new wxBoxSizer(wxHORIZONTAL);
	m_ui.addComboBoxAndLabel(tv_box, "TV Shader (F7):", "TVShader", &theApp.m_gs_tv_shaders);
	shader_box->Add(tv_box);

	tab_box->Add(shader_box, wxSizerFlags().Expand());
	SetSizerAndFit(tab_box);
	Bind(wxEVT_CHECKBOX, &PostTab::CallUpdate, this);
}

void PostTab::CallUpdate(wxCommandEvent& /*event*/)
{
	m_ui.Update();
}

OSDTab::OSDTab(wxWindow* parent)
	: wxPanel(parent, wxID_ANY)
	, m_ui(this)
{
	auto* tab_box = new wxBoxSizer(wxVERTICAL);

	auto* monitor_check = m_ui.addCheckBox(tab_box, "Enable Monitor", "osd_monitor_enabled", IDC_OSD_MONITOR);

	auto* font_box = new wxStaticBoxSizer(wxVERTICAL, this, "Font");
	auto* font_grid = new wxFlexGridSizer(2, 0, 5);
	font_grid->AddGrowableCol(1);

	m_ui.addSpinAndLabel(font_grid, "Size:", "osd_fontsize", 1, 100, 25, -1, monitor_check);

	m_ui.addSliderAndLabel(font_grid, "Red:",     "osd_color_r",       0, 255,   0, -1, monitor_check);
	m_ui.addSliderAndLabel(font_grid, "Green:",   "osd_color_g",       0, 255,   0, -1, monitor_check);
	m_ui.addSliderAndLabel(font_grid, "Blue:",    "osd_color_b",       0, 255,   0, -1, monitor_check);
	m_ui.addSliderAndLabel(font_grid, "Opacity:", "osd_color_opacity", 0, 100, 100, -1, monitor_check);

	font_box->Add(font_grid, wxSizerFlags().Centre().Expand());
	tab_box->Add(font_box, wxSizerFlags().Centre().Expand());

	auto* log_check = m_ui.addCheckBox(tab_box, "Enable Log", "osd_log_enabled", IDC_OSD_LOG);

	auto* log_box = new wxStaticBoxSizer(wxVERTICAL, this, "Log Messages");
	auto* log_grid = new wxFlexGridSizer(2, 5, 5);

	m_ui.addSpinAndLabel(log_grid, "Timeout (seconds):",      "osd_log_timeout",      2, 10, 4,              -1, log_check);
	m_ui.addSpinAndLabel(log_grid, "Max On-Screen Messages:", "osd_max_log_messages", 1, 10, 2, IDC_OSD_MAX_LOG, log_check);

	log_box->Add(log_grid, wxSizerFlags().Centre().Expand());
	tab_box->Add(log_box, wxSizerFlags().Centre());

	SetSizerAndFit(tab_box);
	Bind(wxEVT_CHECKBOX, &OSDTab::CallUpdate, this);
}

void OSDTab::CallUpdate(wxCommandEvent& /*event*/)
{
	m_ui.Update();
}

DebugTab::DebugTab(wxWindow* parent)
	: wxPanel(parent, wxID_ANY)
	, m_ui(this)
{
	auto* tab_box = new wxBoxSizer(wxVERTICAL);

	auto* debug_box = new wxStaticBoxSizer(wxVERTICAL, this, "Debug");
	auto* debug_check_box = new wxWrapSizer(wxHORIZONTAL);
	m_ui.addCheckBox(debug_check_box, "GLSL compilation", "debug_glsl_shader");
	m_ui.addCheckBox(debug_check_box, "Print GL error", "debug_opengl");
	m_ui.addCheckBox(debug_check_box, "Dump GS data", "dump");

	auto* debug_save_check_box = new wxWrapSizer(wxHORIZONTAL);
	m_ui.addCheckBox(debug_save_check_box, "Save RT",      "save");
	m_ui.addCheckBox(debug_save_check_box, "Save Frame",   "savef");
	m_ui.addCheckBox(debug_save_check_box, "Save Texture", "savet");
	m_ui.addCheckBox(debug_save_check_box, "Save Depth",   "savez");

	debug_box->Add(debug_check_box);
	debug_box->Add(debug_save_check_box);

	auto* dump_grid = new wxFlexGridSizer(2, 5, 5);

	start_dump_spin = m_ui.addSpinAndLabel(dump_grid, "Start of Dump:", "saven", 0, pow(10, 9),    0);
	end_dump_spin   = m_ui.addSpinAndLabel(dump_grid, "End of Dump:",   "savel", 0, pow(10, 5), 5000);

	debug_box->AddSpacer(5);
	debug_box->Add(dump_grid);

	auto* ogl_box = new wxStaticBoxSizer(wxVERTICAL, this, "OpenGL");
	auto* ogl_grid = new wxFlexGridSizer(2, 5, 5);
	m_ui.addComboBoxAndLabel(ogl_grid, "Geometry Shader:",  "override_geometry_shader",                &theApp.m_gs_generic_list, IDC_GEOMETRY_SHADER_OVERRIDE);
	m_ui.addComboBoxAndLabel(ogl_grid, "Image Load Store:", "override_GL_ARB_shader_image_load_store", &theApp.m_gs_generic_list, IDC_IMAGE_LOAD_STORE);
	m_ui.addComboBoxAndLabel(ogl_grid, "Sparse Texture:",   "override_GL_ARB_sparse_texture",          &theApp.m_gs_generic_list, IDC_SPARSE_TEXTURE);
	ogl_box->Add(ogl_grid);

	tab_box->Add(debug_box, wxSizerFlags().Centre().Expand());
	tab_box->Add(ogl_box, wxSizerFlags().Centre().Expand());

	SetSizerAndFit(tab_box);
	Bind(wxEVT_SPINCTRL, &DebugTab::CallUpdate, this);
	Bind(wxEVT_CHECKBOX, &DebugTab::CallUpdate, this);
}

void DebugTab::CallUpdate(wxCommandEvent& /*event*/)
{
	Update();
}

void DebugTab::Update()
{
	m_ui.Update();
	if (end_dump_spin->GetValue() < start_dump_spin->GetValue())
		end_dump_spin->SetValue(start_dump_spin->GetValue());
}

Dialog::Dialog()
	: wxDialog(nullptr, wxID_ANY, "GS Config", wxDefaultPosition, wxDefaultSize, wxCAPTION | wxCLOSE_BOX | wxRESIZE_BORDER)
	, m_ui(this)
{
	auto* padding = new wxBoxSizer(wxVERTICAL);
	m_top_box = new wxBoxSizer(wxVERTICAL);

	// Right now, I'm hardcoding the location of the picture.
	wxString fileLocation = wxStandardPaths::Get().GetExecutablePath();
	fileLocation = wxFileName(fileLocation).GetPath() + wxT("/plugins/logo-ogl.bmp");
	if (wxFileName(fileLocation).Exists())
	{
		wxBitmap logo(fileLocation, wxBITMAP_TYPE_BMP);
		m_top_box->Add(new wxStaticBitmap(this, wxID_ANY, logo), wxSizerFlags().Centre());
	}

	auto* top_grid = new wxFlexGridSizer(2, 5, 5);
	top_grid->SetFlexibleDirection(wxHORIZONTAL);

	m_ui.addComboBoxAndLabel(top_grid, "Renderer:", "Renderer", &theApp.m_gs_renderers);

#ifdef _WIN32
	add_label(this, top_grid, "Adapter:");
	wxArrayString m_adapter_str;
	//add_settings_to_array_string(theApp.m_gs_renderers, m_adapter_str);
	m_adapter_str.Add("Default");
	m_adapter_select = new wxChoice(this, wxID_ANY, wxDefaultPosition, wxDefaultSize, m_adapter_str);
	top_grid->Add(m_adapter_select, wxSizerFlags().Expand());
#endif

	m_ui.addComboBoxAndLabel(top_grid, "Interlacing (F5):", "interlace", &theApp.m_gs_interlace);
	m_ui.addComboBoxAndLabel(top_grid, "Texture Filtering:", "filter", &theApp.m_gs_bifilter, IDC_FILTER);

	auto* book = new wxNotebook(this, wxID_ANY, wxDefaultPosition, wxDefaultSize);

	m_renderer_panel = new RendererTab(book);
	m_hacks_panel = new HacksTab(book);
	m_rec_panel = new RecTab(book);
	m_post_panel = new PostTab(book);
	m_osd_panel = new OSDTab(book);
	m_debug_panel = new DebugTab(book);

	book->AddPage(m_renderer_panel, "Renderer", true);
	book->AddPage(m_hacks_panel, "Hacks");
	book->AddPage(m_post_panel, "Shader");
	book->AddPage(m_osd_panel, "OSD");
	book->AddPage(m_rec_panel, "Recording");
	book->AddPage(m_debug_panel, "Debug/OGL");
	book->SetPadding(wxSize(0, 0));

	m_top_box->Add(top_grid, wxSizerFlags().Centre());
	m_top_box->Add(book, wxSizerFlags().Centre().Expand());

	padding->Add(m_top_box, wxSizerFlags().Centre().Expand().Border(wxALL, 5));

	m_top_box->Add(CreateStdDialogButtonSizer(wxOK | wxCANCEL), wxSizerFlags().Right());

	SetSizerAndFit(padding);
	Bind(wxEVT_CHECKBOX, &Dialog::CallUpdate, this);
}

Dialog::~Dialog()
{
}

void Dialog::CallUpdate(wxCommandEvent& /*event*/)
{
	Update();
}

void Dialog::Load()
{
	m_ui.Load();
#ifdef _WIN32
	m_adapter_select->SetSelection(0);
#endif

	m_hacks_panel->Load();
	m_renderer_panel->Load();
	m_rec_panel->Load();
	m_post_panel->Load();
	m_osd_panel->Load();
	m_debug_panel->Load();
}

void Dialog::Save()
{
	m_ui.Save();

	m_hacks_panel->Save();
	m_renderer_panel->Save();
	m_rec_panel->Save();
	m_post_panel->Save();
	m_osd_panel->Save();
	m_debug_panel->Save();
}

void Dialog::Update()
{
	m_ui.Update();
	m_adapter_select->Disable();

	m_hacks_panel->Update();
	m_renderer_panel->Update();
	m_rec_panel->Update();
	m_post_panel->Update();
	m_osd_panel->Update();
	m_debug_panel->Update();
}

bool RunwxDialog()
{
	Dialog GSSettingsDialog;

	GSSettingsDialog.Load();
	if (GSSettingsDialog.ShowModal() == wxID_OK)
		GSSettingsDialog.Save();

	return true;
}
